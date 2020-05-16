// Copyright (c) 2020 Daniel Grunwald

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace NullabilityInference
{
    /// <summary>
    /// Walks the syntax tree and constructs NullabilityEdges.
    /// 
    /// This visitor assumes that the NodeBuildingSyntaxVisitor has already run for all syntax trees in the project,
    /// so that the TypeWithNode for any declaration can be looked up in the typeSystem.
    /// This visitor will:
    ///  * For every `TypeSyntax`, produce the same `TypeWithNode` as `NodeBuildingSyntaxVisitor` did.
    ///  * For every `ExpressionSyntax`, produce a `TypeWithNode` for the expression's type and nullability.
    ///  * Other syntactic nodes return `typeSystem.VoidType`
    /// </summary>
    internal class EdgeBuildingSyntaxVisitor : GraphBuildingSyntaxVisitor
    {
        private readonly SemanticModel semanticModel;
        private readonly TypeSystem typeSystem;
        private readonly CancellationToken cancellationToken;
        private readonly SyntaxToNodeMapping mapping;
        private readonly EdgeBuildingOperationVisitor operationVisitor;

        internal readonly List<NullabilityNode> NewNodes = new List<NullabilityNode>();
        internal readonly List<NullabilityEdge> NewEdges = new List<NullabilityEdge>();

        public EdgeBuildingSyntaxVisitor(SemanticModel semanticModel, TypeSystem typeSystem, SyntaxToNodeMapping mapping, CancellationToken cancellationToken)
        {
            this.semanticModel = semanticModel;
            this.typeSystem = typeSystem;
            this.cancellationToken = cancellationToken;
            this.mapping = mapping;
            this.operationVisitor = new EdgeBuildingOperationVisitor(this, typeSystem);
        }

        public override TypeWithNode DefaultVisit(SyntaxNode node)
        {
            cancellationToken.ThrowIfCancellationRequested();
            if (node is ExpressionSyntax) {
                if (node is TypeSyntax) {
                    throw new NotImplementedException(node.Kind().ToString());
                } else {
                    return HandleAsOperation(node);
                }
            } else if (node is StatementSyntax) {
                return HandleAsOperation(node);
            }

            foreach (var child in node.ChildNodes()) {
                Visit(child);
            }
            return typeSystem.VoidType;
        }

        private TypeWithNode HandleAsOperation(SyntaxNode node)
        {
            var operation = semanticModel.GetOperation(node, cancellationToken);
            if (operation == null)
                throw new NotSupportedException($"Could not get operation for {node}");
            return operation.Accept(operationVisitor, new EdgeBuildingContext());
        }

        internal bool IsNonNullFlow(SyntaxNode syntax)
        {
            var typeInfo = semanticModel.GetTypeInfo(syntax, cancellationToken);
            return typeInfo.Nullability.FlowState == NullableFlowState.NotNull;
        }

        protected override TypeWithNode HandleTypeName(TypeSyntax node, IEnumerable<TypeSyntax>? typeArguments)
        {
            TypeWithNode[]? typeArgs = typeArguments?.Select(s => s.Accept(this)).ToArray();
            var symbolInfo = semanticModel.GetSymbolInfo(node, cancellationToken);
            switch (symbolInfo.Symbol!.Kind) {
                case SymbolKind.NamedType:
                    var ty = (INamedTypeSymbol)symbolInfo.Symbol;
                    if (ty.IsReferenceType && CanBeMadeNullableSyntax(node)) {
                        return new TypeWithNode(ty, mapping[node], typeArgs);
                    } else {
                        return new TypeWithNode(ty, typeSystem.ObliviousNode, typeArgs);
                    }
                case SymbolKind.TypeParameter:
                    var tp = (ITypeParameterSymbol)symbolInfo.Symbol;
                    if (tp.HasReferenceTypeConstraint && CanBeMadeNullableSyntax(node)) {
                        return new TypeWithNode(tp, mapping[node], typeArgs);
                    } else {
                        return new TypeWithNode(tp, typeSystem.ObliviousNode, typeArgs);
                    }
                case SymbolKind.PointerType:
                case SymbolKind.ArrayType:
                    throw new NotImplementedException(symbolInfo.Symbol.Kind.ToString());
                default:
                    return typeSystem.VoidType;
            }
        }

        internal TypeWithNode currentMethodReturnType;

        public override TypeWithNode VisitMethodDeclaration(MethodDeclarationSyntax node)
        {
            var outerMethodReturnType = currentMethodReturnType;
            try {
                var symbol = semanticModel.GetDeclaredSymbol(node);
                if (symbol != null) {
                    currentMethodReturnType = typeSystem.GetSymbolType(symbol);
                } else {
                    currentMethodReturnType = typeSystem.VoidType;
                }
                return HandleAsOperation(node);
            } finally {
                currentMethodReturnType = outerMethodReturnType;
            }
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
            Debug.Assert(SymbolEqualityComparer.Default.Equals(source.Type, target.Type));
            if (source.Type is INamedTypeSymbol namedType) {
                Debug.Assert(source.TypeArguments.Count == namedType.TypeParameters.Length);
                Debug.Assert(target.TypeArguments.Count == namedType.TypeParameters.Length);
                for (int i = 0; i < namedType.TypeParameters.Length; i++) {
                    tp = namedType.TypeParameters[i];
                    var sourceArg = source.TypeArguments[i];
                    var targetArg = target.TypeArguments[i];
                    var combinedVariance = (variance, tp.Variance) switch
                    {
                        (VarianceKind.None, _) => VarianceKind.None,
                        (_, VarianceKind.None) => VarianceKind.None,
                        (VarianceKind.Out, VarianceKind.Out) => VarianceKind.Out,
                        (VarianceKind.In, VarianceKind.Out) => VarianceKind.In,
                        (VarianceKind.Out, VarianceKind.In) => VarianceKind.In,
                        (VarianceKind.In, VarianceKind.In) => VarianceKind.Out,
                        _ => throw new NotSupportedException("Unknown VarianceKind")
                    };
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
        /// Create temporary type nodes for the specified type.
        /// </summary>
        internal TypeWithNode CreateTemporaryType(ITypeSymbol type)
        {
            if (type is INamedTypeSymbol nts) {
                var typeArgs = nts.TypeArguments.Select(CreateTemporaryType).ToArray();
                if (nts.IsReferenceType) {
                    return new TypeWithNode(nts, CreateTemporaryNode(), typeArgs);
                } else {
                    return new TypeWithNode(nts, typeSystem.ObliviousNode, typeArgs);
                }
            } else if (type is IArrayTypeSymbol ats) {
                return new TypeWithNode(ats, CreateTemporaryNode(), new[] { CreateTemporaryType(ats.ElementType) });
            } else if (type is IPointerTypeSymbol pts) {
                return new TypeWithNode(pts, CreateTemporaryNode(), new[] { CreateTemporaryType(pts.PointedAtType) });
            }
            return new TypeWithNode(type, typeSystem.ObliviousNode);
        }

        internal NullabilityNode CreateTemporaryNode()
        {
            var node = new TemporaryNullabilityNode();
            NewNodes.Add(node);
            return node;
        }

        /// <summary>
        /// Creates an edge source->target.
        /// </summary>
        internal NullabilityEdge? CreateEdge(NullabilityNode source, NullabilityNode target)
        {
            // Ignore a bunch of special cases where the edge won't have any effect on the overall result:
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
            NewEdges.Add(edge);
            return edge;
        }
    }
}
