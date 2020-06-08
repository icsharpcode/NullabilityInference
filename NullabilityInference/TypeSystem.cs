// Copyright (c) 2020 Daniel Grunwald
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy of this
// software and associated documentation files (the "Software"), to deal in the Software
// without restriction, including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
// to whom the Software is furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in all copies or
// substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
// PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
// FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Operations;

namespace ICSharpCode.NullabilityInference
{
    public sealed class TypeSystem
    {
        public NullabilityNode NullableNode { get; } = new SpecialNullabilityNode(NullType.Nullable);
        public NullabilityNode NonNullNode { get; } = new SpecialNullabilityNode(NullType.NonNull);
        public NullabilityNode ObliviousNode { get; } = new SpecialNullabilityNode(NullType.Oblivious);

        private readonly CSharpCompilation compilation;
        private readonly Dictionary<SyntaxTree, SyntaxToNodeMapping> syntaxMapping = new Dictionary<SyntaxTree, SyntaxToNodeMapping>();
        private readonly Dictionary<ISymbol, TypeWithNode> symbolType = new Dictionary<ISymbol, TypeWithNode>();
        private readonly Dictionary<(ITypeSymbol, ITypeSymbol), TypeWithNode> baseTypes = new Dictionary<(ITypeSymbol, ITypeSymbol), TypeWithNode>();
        private readonly List<NullabilityNode> additionalNodes = new List<NullabilityNode>();


        private readonly INamedTypeSymbol voidType;
        public TypeWithNode VoidType => new TypeWithNode(voidType, ObliviousNode);

        public TypeSystem(CSharpCompilation compilation)
        {
            this.compilation = compilation;
            this.voidType = compilation.GetSpecialType(SpecialType.System_Void);
        }

        public CSharpCompilation Compilation => compilation;

        /// <summary>
        /// Adjusts the symbol to use for GetSymbolType() calls.
        /// This maps parameters from accessors to the corresponding event parameter.
        /// </summary>
        internal static ISymbol SymbolAdjustments(ISymbol symbol)
        {
            if (symbol is IParameterSymbol { ContainingSymbol: IMethodSymbol { AssociatedSymbol: var assoc } } p) {
                // A parameter on an accessor differs from the parameter on the surrounding indexer.
                if (assoc is IPropertySymbol prop) {
                    if (p.Ordinal >= prop.Parameters.Length) {
                        Debug.Assert(p.Name == "value");
                        Debug.Assert(p.Ordinal == prop.Parameters.Length);
                        // 'value' in property setter has same type as property return type
                        return prop;
                    } else {
                        return prop.Parameters[p.Ordinal];
                    }
                } else if (assoc is IEventSymbol ev) {
                    Debug.Assert(p.Name == "value");
                    Debug.Assert(p.Ordinal == 0);
                    return ev;
                }
            } else if (symbol is IMethodSymbol { MethodKind: MethodKind.PropertyGet, AssociatedSymbol: IPropertySymbol prop2 }) {
                return prop2;
            } else if (symbol is IFieldSymbol { IsImplicitlyDeclared: true } fieldSymbol) {
                return fieldSymbol.AssociatedSymbol ?? symbol;
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
            Debug.Assert(!SymbolEqualityComparer.Default.Equals(symbol.ContainingModule, compilation.SourceModule)
                || IsHarmlessIgnoredSymbol(symbol),
                "Symbols from the SourceModule should be found in the symbolType dictionary.");
            switch (symbol.Kind) {
                case SymbolKind.Method:
                    var method = (IMethodSymbol)symbol;
                    return FromType(method.ReturnType, method.ReturnNullableAnnotation, method.GetReturnTypeAttributes());
                case SymbolKind.Parameter:
                    var parameter = (IParameterSymbol)symbol;
                    return FromType(parameter.Type, parameter.NullableAnnotation, parameter.GetAttributes());
                case SymbolKind.Property:
                    var property = (IPropertySymbol)symbol;
                    return FromType(property.Type, property.NullableAnnotation, property.GetAttributes());
                case SymbolKind.Field:
                    var field = (IFieldSymbol)symbol;
                    return FromType(field.Type, field.NullableAnnotation, field.GetAttributes());
                case SymbolKind.Event:
                    var ev = (IEventSymbol)symbol;
                    return FromType(ev.Type, ev.NullableAnnotation, ev.GetAttributes());
                default:
                    throw new NotImplementedException($"External symbol: {symbol.Kind}");
            }
        }

        private static bool IsHarmlessIgnoredSymbol(ISymbol symbol)
        {
            // Some symbols aren't registered in our type system,
            // e.g. property setters or event accessors.
            // But those have return type void, so we can use the FromType() code path without harm.
            if (symbol is IMethodSymbol method) {
                return method.ReturnsVoid;
            }
            return false;
        }

        internal TypeWithNode FromType(ITypeSymbol? type, NullableAnnotation nullability, ImmutableArray<AttributeData> attributeData = default)
        {
            var topLevelNode = nullability switch
            {
                NullableAnnotation.Annotated => NullableNode,
                NullableAnnotation.NotAnnotated => NonNullNode,
                _ => ObliviousNode,
            };
            if (!attributeData.IsDefaultOrEmpty) {
                foreach (var attr in attributeData) {
                    switch (attr.AttributeClass?.GetFullName()) {
                        case "System.Diagnostics.CodeAnalysis.MaybeNullAttribute":
                        case "System.Diagnostics.CodeAnalysis.MaybeNullWhenAttribute":
                            topLevelNode = NullableNode;
                            break;
                    }
                }
            }
            if (type is INamedTypeSymbol nts) {
                return new TypeWithNode(nts, topLevelNode, nts.FullTypeArguments().Zip(nts.FullTypeArgumentNullableAnnotations(), (a, b) => FromType(a, b)).ToArray());
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
                return new TypeWithNode(nts, ObliviousNode, nts.FullTypeArguments().Select(this.GetObliviousType).ToArray());
            } else if (type is IArrayTypeSymbol ats) {
                return new TypeWithNode(ats, ObliviousNode, new[] { GetObliviousType(ats.ElementType) });
            } else if (type is IPointerTypeSymbol pts) {
                return new TypeWithNode(pts, ObliviousNode, new[] { GetObliviousType(pts.PointedAtType) });
            }
            return new TypeWithNode(type, ObliviousNode);
        }

        private TypeWithNode GetDirectBase(ITypeSymbol derivedTypeDef, INamedTypeSymbol baseType, INamedTypeSymbol baseTypeInstance, TypeSubstitution substitution)
        {
            // Given:
            //   derivedType = Dictionary<TKey, TValue>
            //   baseType = IEnumerable<KeyValuePair<TKey, TValue>>
            //   baseTypeInstance = IEnumerable<KeyValuePair<string, string>>
            //   substitution = {TKey: string#1, TValue: string#2}
            // Returns `IEnumerable<KeyValuePair<string#1, string#2>>`
            if (!baseTypes.TryGetValue((derivedTypeDef, baseType), out var typeWithNode)) {
                typeWithNode = FromType(baseType, NullableAnnotation.None);
            }
            return typeWithNode.WithSubstitution(baseTypeInstance, substitution);
        }

        /// <summary>
        /// Gets the `TypeWithNode` for the direct base types (including interface types) of derivedType.
        /// </summary>
        private IEnumerable<TypeWithNode> GetDirectBases(TypeWithNode derivedType, bool includeInterfaces = false)
        {
            if (derivedType.Type == null)
                yield break;
            var substitution = new TypeSubstitution(derivedType.TypeArguments, new TypeWithNode[0]);
            var derivedTypeDef = derivedType.Type.OriginalDefinition;
            var baseType = derivedTypeDef.BaseType;
            var baseTypeInstance = derivedType.Type.BaseType;
            if (baseType != null && baseTypeInstance != null) {
                yield return GetDirectBase(derivedTypeDef, baseType, baseTypeInstance, substitution);
            }
            if (includeInterfaces) {
                foreach (var (interfaceType, interfaceTypeInstance) in derivedTypeDef.Interfaces.Zip(derivedType.Type.Interfaces)) {
                    yield return GetDirectBase(derivedTypeDef, interfaceType, interfaceTypeInstance, substitution);
                }
            }
        }

        /// <summary>
        /// Gets all base classes (and optionally interfaces) implemented by the derived type.
        /// Includes the derived type itself (unless it's an array type).
        /// </summary>
        internal IEnumerable<TypeWithNode> GetAllBases(TypeWithNode derivedType, bool includeInterfaces = false)
        {
            if (derivedType.Type == null) {
                yield break;
            }
            if (derivedType.Type is IArrayTypeSymbol arrayType) {
                // For arrays there's no good way to map from `string[]` to `T[]` so that we could substitute `T=string` back into `IEnumerable<T>`.
                // So just special-case arrays completely:
                Debug.Assert(derivedType.TypeArguments.Count == 1);
                if (includeInterfaces) {
                    foreach (var arrayInterface in arrayType.AllInterfaces) {
                        Debug.Assert(arrayInterface.Arity <= 1);
                        if (arrayInterface.Arity == 0) {
                            // non-generic interface implemented by System.Array
                            yield return new TypeWithNode(arrayInterface, derivedType.Node);
                        } else {
                            // generic interface implemented by System.Array -> type argument will be the array's element type
                            yield return new TypeWithNode(arrayInterface, derivedType.Node, derivedType.TypeArguments);
                        }
                    }
                }
                for (INamedTypeSymbol? baseType = arrayType.BaseType; baseType != null; baseType = baseType.BaseType) {
                    yield return new TypeWithNode(baseType, derivedType.Node);
                }
                yield break;
            }
            var visited = new HashSet<ITypeSymbol>(SymbolEqualityComparer.Default);
            var worklist = new Stack<TypeWithNode>();
            visited.Add(derivedType.Type);
            worklist.Push(derivedType);
            while (worklist.Count > 0) {
                derivedType = worklist.Pop();
                yield return derivedType;
                foreach (var baseType in GetDirectBases(derivedType, includeInterfaces)) {
                    if (baseType.Type == null)
                        continue;
                    if (visited.Add(baseType.Type)) {
                        worklist.Push(baseType);
                    }
                }
            }
        }

        /// <summary>
        /// Gets the `TypeWithNode` for the specified base type (or specified interface) of the derivedType.
        /// </summary>
        internal TypeWithNode? GetBaseType(TypeWithNode derivedType, INamedTypeSymbol desiredBaseType)
        {
            // Example:
            //   derivedType = Dictionary<string#1, string#2>#3
            //   baseTypeDefinition = IEnumerable
            // ->
            //   return IEnumerable<KeyValuePair<string#1, string#2>>#3
            TypeWithNode? fallbackResult = null;
            foreach (var baseType in GetAllBases(derivedType, includeInterfaces: desiredBaseType.TypeKind == TypeKind.Interface)) {
                if (SymbolEqualityComparer.Default.Equals(baseType.Type, desiredBaseType)) {
                    return baseType;
                } else if (SymbolEqualityComparer.Default.Equals(baseType.Type?.OriginalDefinition, desiredBaseType)) {
                    fallbackResult = baseType;
                }
            }
            return fallbackResult;
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
                    if (sym is IParameterSymbol param) {
                        VarianceKind expectedVariance = VarianceKind.Out;
                        if (param.RefKind == RefKind.Out) {
                            expectedVariance = VarianceKind.In;
                        } else if (param.RefKind != RefKind.None) {
                            continue;
                        }
                        foreach (var (node, variance) in type.NodesWithVariance()) {
                            if (variance == expectedVariance) {
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

            public void AddBaseType(INamedTypeSymbol derivedType, TypeWithNode baseType)
            {
                if (baseType.Type == null)
                    return;
                Debug.Assert(SymbolEqualityComparer.Default.Equals(derivedType, derivedType.OriginalDefinition));
                var key = (derivedType.OriginalDefinition, baseType.Type);
                AddAction(ts => ts.baseTypes[key] = baseType);
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
                    edge.Target.ResidualGraphPredecessors.Add(edge.Source);
                }
                newEdges.Clear();
            }

            /// <summary>
            /// Create temporary type nodes for the specified type.
            /// </summary>
            public TypeWithNode CreateTemporaryType(ITypeSymbol? type)
            {
                if (type == null)
                    return VoidType;
                if (type is INamedTypeSymbol nts) {
                    var typeArgs = nts.FullTypeArguments().Select(CreateTemporaryType).ToArray();
                    if (nts.IsReferenceType) {
                        return new TypeWithNode(nts, CreateTemporaryNode(), typeArgs);
                    } else {
                        return new TypeWithNode(nts, ObliviousNode, typeArgs);
                    }
                } else if (type is IArrayTypeSymbol ats) {
                    return new TypeWithNode(ats, CreateTemporaryNode(), new[] { CreateTemporaryType(ats.ElementType) });
                } else if (type is IPointerTypeSymbol pts) {
                    return new TypeWithNode(pts, CreateTemporaryNode(), new[] { CreateTemporaryType(pts.PointedAtType) });
                } else if (type is ITypeParameterSymbol tp && tp.CanBeMadeNullable()) {
                    return new TypeWithNode(tp, CreateTemporaryNode());
                }
                return new TypeWithNode(type, ObliviousNode);
            }

            public NullabilityNode CreateTemporaryNode()
            {
                var node = new TemporaryNullabilityNode();
                newNodes.Add(node);
                return node;
            }

            internal void CreateAssignmentEdge(TypeWithNode source, TypeWithNode target, EdgeLabel label)
            {
                CreateTypeEdge(source, target, null, VarianceKind.Out, label);
            }

            internal void CreateTypeEdge(TypeWithNode source, TypeWithNode target, TypeSubstitution? targetSubstitution, VarianceKind variance, EdgeLabel label)
            {
                if (targetSubstitution != null && target.Type is ITypeParameterSymbol tp) {
                    // If calling `void SomeCall<T>(T x);` as `SomeCall<string>(null)`, then
                    // we need either `x: T?` or `T = string?`:
                    //   (source is nullable) implies (target is nullable || substitutedTarget is nullable)
                    // We can't represent such a choice in the graph, so we always substitute and prefer `string?`.

                    // However, if the variance causes us to create edges the other way around
                    // (e.g. an override-edge for `override void SomeCall(string x)`), we have:
                    //   (target is nullable || substitutedTarget is nullable) implies (source is nullable)
                    // This can be represented by using two edges.
                    if (variance == VarianceKind.In || variance == VarianceKind.None) {
                        CreateEdge(target.Node, source.Node, label);
                    }

                    // Perform the substitution:
                    target = targetSubstitution.Value[tp.TypeParameterKind, tp.FullOrdinal()];
                    targetSubstitution = null;
                }
                Debug.Assert(source.Type?.TypeKind == target.Type?.TypeKind);
                if (source.Type is INamedTypeSymbol namedType) {
                    if (!SymbolEqualityComparer.Default.Equals(source.Type?.OriginalDefinition, target.Type?.OriginalDefinition)) {
                        throw new InvalidOperationException($"Types don't match: {source.Type} vs. {target.Type}");
                    }
                    var namedTypeTypeParameters = namedType.FullTypeParameters().ToList();
                    Debug.Assert(source.TypeArguments.Count == namedTypeTypeParameters.Count);
                    Debug.Assert(target.TypeArguments.Count == namedTypeTypeParameters.Count);
                    for (int i = 0; i < namedTypeTypeParameters.Count; i++) {
                        var sourceArg = source.TypeArguments[i];
                        var targetArg = target.TypeArguments[i];
                        var combinedVariance = (variance, namedTypeTypeParameters[i].Variance).Combine();
                        CreateTypeEdge(sourceArg, targetArg, targetSubstitution, combinedVariance, label);
                    }
                } else if (source.Type is IArrayTypeSymbol || source.Type is IPointerTypeSymbol) {
                    CreateTypeEdge(source.TypeArguments.Single(), target.TypeArguments.Single(), targetSubstitution, variance, label);
                }
                if (variance == VarianceKind.In || variance == VarianceKind.None)
                    CreateEdge(target.Node, source.Node, label);
                if (variance == VarianceKind.Out || variance == VarianceKind.None)
                    CreateEdge(source.Node, target.Node, label);
            }

            /// <summary>
            /// Creates an edge source->target.
            /// </summary>
            public NullabilityEdge? CreateEdge(NullabilityNode source, NullabilityNode target, EdgeLabel label)
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
                var edge = new NullabilityEdge(source, target, label);
                //Debug.WriteLine($"New edge: {source.Name} -> {target.Name}");
                newEdges.Add(edge);
                return edge;
            }
        }
    }
}
