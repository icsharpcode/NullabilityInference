// Copyright (c) 2020 Daniel Grunwald

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using Microsoft.CodeAnalysis;

namespace NullabilityInference
{
    public sealed class TypeSystem
    {
        public NullabilityNode NullableNode { get; } = new SpecialNullabilityNode(NullType.Nullable);
        public NullabilityNode NonNullNode { get; } = new SpecialNullabilityNode(NullType.NonNull);
        public NullabilityNode ObliviousNode { get; } = new SpecialNullabilityNode(NullType.Oblivious);

        private readonly Compilation compilation;
        private readonly Dictionary<SyntaxTree, SyntaxToNodeMapping> syntaxMapping = new Dictionary<SyntaxTree, SyntaxToNodeMapping>();
        private readonly Dictionary<ISymbol, TypeWithNode> symbolType = new Dictionary<ISymbol, TypeWithNode>();
        private readonly List<NullabilityNode> additionalNodes = new List<NullabilityNode>();


        private readonly INamedTypeSymbol voidType;
        public TypeWithNode VoidType => new TypeWithNode(voidType, ObliviousNode);

        public TypeSystem(Compilation compilation)
        {
            this.compilation = compilation;
            this.voidType = compilation.GetSpecialType(SpecialType.System_Void);
        }


        /// <summary>
        /// Adjusts the symbol to use for GetSymbolType() calls.
        /// This maps parameters from accessors to the corresponding event parameter.
        /// </summary>
        internal static ISymbol SymbolAdjustments(ISymbol symbol)
        {
            if (symbol is IParameterSymbol { ContainingSymbol: IMethodSymbol { AssociatedSymbol: IPropertySymbol prop } } p) {
                // A parameter on an accessor differs from the parameter on the surrounding indexer.
                if (p.Ordinal >= prop.Parameters.Length) {
                    Debug.Assert(p.Name == "value");
                    Debug.Assert(p.Ordinal == prop.Parameters.Length);
                    // 'value' in property setter has same type as property return type
                    return prop;
                } else {
                    return prop.Parameters[p.Ordinal];
                }
            }
            return symbol;
        }

        public TypeWithNode GetSymbolType(ISymbol symbol)
        {
            symbol = SymbolAdjustments(symbol);
            if (symbolType.TryGetValue(symbol, out var type)) {
                Debug.Assert(SymbolEqualityComparer.Default.Equals(symbol.ContainingModule, compilation.SourceModule),
                    "Entries in the symbolType dictionary should be from the SourceModule.");
                return type;
            }
            Debug.Assert(!SymbolEqualityComparer.Default.Equals(symbol.ContainingModule, compilation.SourceModule),
                "Symbols from the SourceModule should be found in the symbolType dictionary.");
            switch (symbol.Kind) {
                case SymbolKind.Method:
                    var method = (IMethodSymbol)symbol;
                    return FromType(method.ReturnType, method.ReturnNullableAnnotation);
                case SymbolKind.Parameter:
                    var parameter = (IParameterSymbol)symbol;
                    return FromType(parameter.Type, parameter.NullableAnnotation);
                case SymbolKind.Property:
                    var property = (IPropertySymbol)symbol;
                    return FromType(property.Type, property.NullableAnnotation);
                case SymbolKind.Field:
                    var field = (IFieldSymbol)symbol;
                    return FromType(field.Type, field.NullableAnnotation);
                case SymbolKind.Event:
                    var ev = (IEventSymbol)symbol;
                    return FromType(ev.Type, ev.NullableAnnotation);
                default:
                    throw new NotImplementedException($"External symbol: {symbol.Kind}");
            }
        }

        internal TypeWithNode FromType(ITypeSymbol? type, NullableAnnotation nullability)
        {
            var topLevelNode = nullability switch
            {
                NullableAnnotation.Annotated => NullableNode,
                NullableAnnotation.NotAnnotated => NonNullNode,
                _ => ObliviousNode,
            };
            if (type is INamedTypeSymbol nts) {
                return new TypeWithNode(nts, topLevelNode, nts.TypeArguments.Zip(nts.TypeArgumentNullableAnnotations, FromType).ToArray());
            } else if (type is IArrayTypeSymbol ats) {
                return new TypeWithNode(ats, topLevelNode, new[] { FromType(ats.ElementType, ats.ElementNullableAnnotation) });
            } else if (type is IPointerTypeSymbol pts) {
                // The pointed-at-type must be a value type, but that value type might have its own type arguments
                return new TypeWithNode(pts, topLevelNode, new[] { FromType(pts.PointedAtType, NullableAnnotation.None) });
            }
            return new TypeWithNode(type, topLevelNode);
        }
        internal TypeWithNode GetObliviousType(ITypeSymbol type)
        {
            if (type is INamedTypeSymbol nts) {
                return new TypeWithNode(nts, ObliviousNode, nts.TypeArguments.Select(this.GetObliviousType).ToArray());
            } else if (type is IArrayTypeSymbol ats) {
                return new TypeWithNode(ats, ObliviousNode, new[] { GetObliviousType(ats.ElementType) });
            } else if (type is IPointerTypeSymbol pts) {
                return new TypeWithNode(pts, ObliviousNode, new[] { GetObliviousType(pts.PointedAtType) });
            }
            return new TypeWithNode(type, ObliviousNode);
        }

        public IEnumerable<NullabilityNode> AllNodes {
            get {
                yield return NullableNode;
                yield return NonNullNode;
                yield return ObliviousNode;
                foreach (var mapping in syntaxMapping.Values) {
                    foreach (var node in mapping.Nodes) {
                        yield return node;
                    }
                }
                foreach (var node in additionalNodes) {
                    yield return node;
                }
            }
        }

        /// <summary>
        /// Gets nullability nodes for parameters.
        /// </summary>
        internal IEnumerable<NullabilityNode> NodesInInputPositions {
            get {
                foreach (var (sym, type) in symbolType) {
                    if (sym.Kind == SymbolKind.Parameter) {
                        foreach (var (node, variance) in type.NodesWithVariance()) {
                            if (variance == VarianceKind.Out) {
                                yield return node;
                            }
                        }
                    } else if (sym.Kind == SymbolKind.Method) {
                        foreach (var (node, variance) in type.NodesWithVariance()) {
                            if (variance == VarianceKind.In) {
                                yield return node;
                            }
                        }
                    }
                }
            }
        }

        internal void RegisterNodes(SyntaxTree syntaxTree, SyntaxToNodeMapping mapping)
        {
            syntaxMapping.Add(syntaxTree, mapping);
        }

        internal SyntaxToNodeMapping GetMapping(SyntaxTree syntaxTree)
        {
            return syntaxMapping[syntaxTree];
        }

        /// <summary>
        /// Caches additions to the type system, actually adding them when Flush() is called.
        /// </summary>
        /// <remarks>
        /// Neither the type-system nor the builder is thread-safe.
        /// However, multiple builders can be used concurrently
        /// </remarks>
        internal class Builder
        {
            public readonly NullabilityNode NullableNode;
            public readonly NullabilityNode NonNullNode;
            public readonly NullabilityNode ObliviousNode;
            public readonly TypeWithNode VoidType;

            public Builder(TypeSystem typeSystem)
            {
                // Don't store the typeSystem is this; we may not access it outside of Flush().
                this.NullableNode = typeSystem.NullableNode;
                this.NonNullNode = typeSystem.NonNullNode;
                this.ObliviousNode = typeSystem.ObliviousNode;
                this.VoidType = typeSystem.VoidType;
            }

            public void AddSymbolType(ISymbol symbol, TypeWithNode type)
            {
                type.SetName(symbol.Name);
                AddAction(ts => ts.symbolType.Add(symbol, type));
            }

            private readonly List<Action<TypeSystem>> cachedActions = new List<Action<TypeSystem>>();
            private readonly List<TemporaryNullabilityNode> newNodes = new List<TemporaryNullabilityNode>();
            private readonly List<NullabilityEdge> newEdges = new List<NullabilityEdge>();

            private void AddAction(Action<TypeSystem> action)
            {
                cachedActions.Add(action);
            }

            public void Flush(TypeSystem typeSystem)
            {
                foreach (var action in cachedActions) {
                    action(typeSystem);
                }
                cachedActions.Clear();
                
                typeSystem.additionalNodes.AddRange(newNodes);
                newNodes.Clear();

                foreach (var edge in newEdges) {
                    Debug.Assert(edge.Source.ReplacedWith == edge.Source);
                    Debug.Assert(edge.Target.ReplacedWith == edge.Target);
                    edge.Source.OutgoingEdges.Add(edge);
                    edge.Target.IncomingEdges.Add(edge);
                }
                newEdges.Clear();
            }

            /// <summary>
            /// Create temporary type nodes for the specified type.
            /// </summary>
            public TypeWithNode CreateTemporaryType(ITypeSymbol type)
            {
                if (type is INamedTypeSymbol nts) {
                    var typeArgs = nts.TypeArguments.Select(CreateTemporaryType).ToArray();
                    if (nts.IsReferenceType) {
                        return new TypeWithNode(nts, CreateTemporaryNode(), typeArgs);
                    } else {
                        return new TypeWithNode(nts, ObliviousNode, typeArgs);
                    }
                } else if (type is IArrayTypeSymbol ats) {
                    return new TypeWithNode(ats, CreateTemporaryNode(), new[] { CreateTemporaryType(ats.ElementType) });
                } else if (type is IPointerTypeSymbol pts) {
                    return new TypeWithNode(pts, CreateTemporaryNode(), new[] { CreateTemporaryType(pts.PointedAtType) });
                }
                return new TypeWithNode(type, ObliviousNode);
            }

            public NullabilityNode CreateTemporaryNode()
            {
                var node = new TemporaryNullabilityNode();
                newNodes.Add(node);
                return node;
            }

            internal NullabilityEdge? CreateAssignmentEdge(TypeWithNode source, TypeWithNode target)
            {
                return CreateTypeEdge(source, target, null, VarianceKind.Out);
            }

            internal NullabilityEdge? CreateTypeEdge(TypeWithNode source, TypeWithNode target, TypeSubstitution? targetSubstitution, VarianceKind variance)
            {
                if (targetSubstitution != null && target.Type is ITypeParameterSymbol tp) {
                    // Perform the substitution:
                    target = targetSubstitution.Value[tp.TypeParameterKind, tp.Ordinal];
                    targetSubstitution = null;
                }
                if (!SymbolEqualityComparer.Default.Equals(source.Type?.OriginalDefinition, target.Type?.OriginalDefinition)) {
                    throw new InvalidOperationException($"Types don't match: {source.Type} vs. {target.Type}");
                }
                if (source.Type is INamedTypeSymbol namedType) {
                    Debug.Assert(source.TypeArguments.Count == namedType.TypeParameters.Length);
                    Debug.Assert(target.TypeArguments.Count == namedType.TypeParameters.Length);
                    for (int i = 0; i < namedType.TypeParameters.Length; i++) {
                        tp = namedType.TypeParameters[i];
                        var sourceArg = source.TypeArguments[i];
                        var targetArg = target.TypeArguments[i];
                        var combinedVariance = (variance, tp.Variance).Combine();
                        CreateTypeEdge(sourceArg, targetArg, targetSubstitution, combinedVariance);
                    }
                } else if (source.Type is IArrayTypeSymbol || source.Type is IPointerTypeSymbol) {
                    CreateTypeEdge(source.TypeArguments.Single(), target.TypeArguments.Single(), targetSubstitution, variance);
                }
                if (variance == VarianceKind.In || variance == VarianceKind.None)
                    CreateEdge(target.Node, source.Node);
                if (variance == VarianceKind.Out || variance == VarianceKind.None)
                    return CreateEdge(source.Node, target.Node);
                else
                    return null;
            }

            /// <summary>
            /// Creates an edge source->target.
            /// </summary>
            public NullabilityEdge? CreateEdge(NullabilityNode source, NullabilityNode target)
            {
                // Ignore a bunch of special cases where the edge won't have any effect on the overall result:
                source = source.ReplacedWith;
                target = target.ReplacedWith;
                if (source == target) {
                    return null;
                }
                if (source.NullType == NullType.NonNull || source.NullType == NullType.Oblivious) {
                    return null;
                }
                if (target.NullType == NullType.Nullable || target.NullType == NullType.Oblivious) {
                    return null;
                }
                var edge = new NullabilityEdge(source, target);
                Debug.WriteLine($"New edge: {source.Name} -> {target.Name}");
                newEdges.Add(edge);
                return edge;
            }
        }
    }
}
