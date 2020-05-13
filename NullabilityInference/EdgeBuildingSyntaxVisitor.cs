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
                default:
                    throw new NotImplementedException(symbolInfo.Symbol.Kind.ToString());
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
            Debug.Assert(SymbolEqualityComparer.Default.Equals(source.Type, target.Type));
            if (source.Type is INamedTypeSymbol namedType) {
                Debug.Assert(source.TypeArguments.Count == namedType.TypeParameters.Length);
                Debug.Assert(target.TypeArguments.Count == namedType.TypeParameters.Length);
                for (int i = 0; i < namedType.TypeParameters.Length; i++) {
                    var tp = namedType.TypeParameters[i];
                    var sourceArg = source.TypeArguments[i];
                    var targetArg = target.TypeArguments[i];
                    switch (tp.Variance) {
                        case VarianceKind.None:
                            // List<string> --/--> IEnumerable<string?>
                            CreateBidirectionalEdge(sourceArg, targetArg);
                            break;
                        case VarianceKind.Out:
                            // IEnumerable<string> --> IEnumerable<string?>
                            CreateAssignmentEdge(sourceArg, targetArg);
                            break;
                        case VarianceKind.In:
                            // IComparable<string?> --> IComparable<string>
                            CreateAssignmentEdge(targetArg, sourceArg);
                            break;
                    }
                }
            }
            return CreateEdge(source.Node, target.Node);
        }

        private void CreateBidirectionalEdge(TypeWithNode source, TypeWithNode target)
        {
            CreateEdge(source.Node, target.Node);
            CreateEdge(target.Node, source.Node);

            Debug.Assert(SymbolEqualityComparer.Default.Equals(source.Type, target.Type));
            Debug.Assert(source.TypeArguments.Count == target.TypeArguments.Count);
            foreach (var (s, t) in source.TypeArguments.Zip(target.TypeArguments)) {
                CreateBidirectionalEdge(s, t);
            }
        }

        private void Dereference(TypeWithNode type, in SyntaxToken derefToken)
        {
            var edge = CreateEdge(type.Node, typeSystem.NonNullNode);
            edge?.SetLabel("Deref", derefToken.GetLocation());
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
            lock (source.OutgoingEdges) source.OutgoingEdges.Add(edge);
            lock (target.IncomingEdges) target.IncomingEdges.Add(edge);
            return edge;
        }
    }
}
