using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
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

        public EdgeBuildingSyntaxVisitor(SemanticModel semanticModel, TypeSystem typeSystem, SyntaxToNodeMapping mapping, CancellationToken cancellationToken)
        {
            this.semanticModel = semanticModel;
            this.typeSystem = typeSystem;
            this.cancellationToken = cancellationToken;
            this.mapping = mapping;
        }

        public override TypeWithNode DefaultVisit(SyntaxNode node)
        {
            cancellationToken.ThrowIfCancellationRequested();
            if (node is ExpressionSyntax)
                throw new NotImplementedException(node.Kind().ToString());
            foreach (var child in node.ChildNodes()) {
                Visit(child);
            }
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitIdentifierName(IdentifierNameSyntax node)
        {
            var symbolInfo = semanticModel.GetSymbolInfo(node, cancellationToken);
            var typeInfo = semanticModel.GetTypeInfo(node, cancellationToken);
            if (symbolInfo.Symbol == null) {
                throw new NotImplementedException("symbolInfo.Symbol == null");
            } else {
                switch (symbolInfo.Symbol.Kind) {
                    case SymbolKind.Namespace:
                        return typeSystem.VoidType;
                    case SymbolKind.Field:
                    case SymbolKind.Parameter:
                    case SymbolKind.Local:
                        return typeSystem.GetSymbolType(symbolInfo.Symbol);
                    case SymbolKind.NamedType:
                        var ty = (ITypeSymbol)symbolInfo.Symbol;
                        //if (ty.IsValueType) {
                        //  return new TypeWithNode(ty, typeSystem.ObliviousNode);
                        //}
                        return new TypeWithNode(ty, mapping[node]);
                    default:
                        // TODO:
                        return typeSystem.FromType(typeInfo.Type, typeInfo.Nullability.Annotation);
                }
            }
        }

        public override TypeWithNode VisitPredefinedType(PredefinedTypeSyntax node)
        {
            var symbolInfo = semanticModel.GetSymbolInfo(node, cancellationToken);
            switch (symbolInfo.Symbol!.Kind) {
                case SymbolKind.NamedType:
                    var ty = (INamedTypeSymbol)symbolInfo.Symbol;
                    if (ty.IsValueType) {
                        return new TypeWithNode(ty, typeSystem.ObliviousNode);
                    }
                    return new TypeWithNode(ty, mapping[node]);
                default:
                    throw new NotImplementedException(symbolInfo.Symbol.Kind.ToString());
            }
        }

        public override TypeWithNode VisitLiteralExpression(LiteralExpressionSyntax node)
        {
            var typeInfo = semanticModel.GetTypeInfo(node, cancellationToken);
            if (typeInfo.Type?.IsValueType == true) {
                return new TypeWithNode(typeInfo.Type, typeSystem.ObliviousNode);
            }
            switch (node.Kind()) {
                case SyntaxKind.StringLiteralExpression:
                    return new TypeWithNode(typeInfo.Type, typeSystem.NonNullNode);
                case SyntaxKind.NullLiteralExpression:
                    return new TypeWithNode(typeInfo.Type, typeSystem.NullableNode);
                default:
                    throw new NotImplementedException(node.Kind().ToString());
            }
        }

        public override TypeWithNode VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
        {
            var target = node.Expression.Accept(this);
            Dereference(target, node.OperatorToken);
            return node.Name.Accept(this);
        }

        public override TypeWithNode VisitObjectCreationExpression(ObjectCreationExpressionSyntax node)
        {
            var symbolInfo = semanticModel.GetSymbolInfo(node, cancellationToken);
            if (symbolInfo.Symbol == null) {
                throw new NotSupportedException("Constructor symbol not found");
            }
            var type = node.Type.Accept(this);
            var args = node.ArgumentList?.Arguments.Select(arg => arg.Accept(this)).ToArray() ?? new TypeWithNode[0];
            Debug.Assert(symbolInfo.Symbol.Kind == SymbolKind.Method);
            var ctor = (IMethodSymbol)symbolInfo.Symbol;
            HandleArgumentsForCall(node.ArgumentList, ctor);
            node.Initializer?.Accept(this);
            return type;
        }

        public override TypeWithNode VisitInvocationExpression(InvocationExpressionSyntax node)
        {
            var symbolInfo = semanticModel.GetSymbolInfo(node, cancellationToken);
            if (symbolInfo.Symbol == null) {
                throw new NotSupportedException("Method symbol not found");
            }
            Debug.Assert(symbolInfo.Symbol.Kind == SymbolKind.Method);
            var method = (IMethodSymbol)symbolInfo.Symbol;
            HandleArgumentsForCall(node.ArgumentList, method);
            if (method.ReducedFrom != null && node.Expression is MemberAccessExpressionSyntax memberAccess) {
                // extension method invocation
                var receiverType = Visit(memberAccess.Expression);
                var thisParam = method.ReducedFrom.Parameters.First();
                var edge = CreateAssignmentEdge(source: receiverType, target: typeSystem.GetSymbolType(thisParam));
                edge?.SetLabel("extension this", memberAccess.OperatorToken.GetLocation());
                return typeSystem.GetSymbolType(method.ReducedFrom);
            }

            var delegateType = node.Expression.Accept(this);
            Dereference(delegateType, node.ArgumentList.OpenParenToken);
            return typeSystem.GetSymbolType(method);
        }

        private void HandleArgumentsForCall(ArgumentListSyntax? argumentList, IMethodSymbol method)
        {
            if (argumentList == null)
                return;
            foreach (var (param, arg) in method.Parameters.Zip(argumentList.Arguments)) {
                if (arg.NameColon != null) {
                    throw new NotImplementedException("Named arguments");
                }
                var paramType = typeSystem.GetSymbolType(param);
                var argType = Visit(arg.Expression);
                var edge = CreateAssignmentEdge(source: argType, target: paramType);
                edge?.SetLabel("Argument", arg.GetLocation());
            }
        }

        public override TypeWithNode VisitVariableDeclarator(VariableDeclaratorSyntax node)
        {
            var symbol = semanticModel.GetDeclaredSymbol(node, cancellationToken);
            node.ArgumentList?.Accept(this);
            if (symbol != null && node.Initializer != null) {
                var valueType = Visit(node.Initializer.Value);
                var symbolType = typeSystem.GetSymbolType(symbol);
                var edge = CreateAssignmentEdge(source: valueType, target: symbolType);
                edge?.SetLabel("VarInit", node.Initializer.GetLocation());
            }
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitThrowExpression(ThrowExpressionSyntax node)
        {
            var exception = Visit(node.Expression);
            Dereference(exception, node.ThrowKeyword);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitAssignmentExpression(AssignmentExpressionSyntax node)
        {
            var lhs = Visit(node.Left);
            var rhs = Visit(node.Right);
            Debug.Assert(SymbolEqualityComparer.Default.Equals(lhs.Type, rhs.Type));
            var edge = CreateAssignmentEdge(source: rhs, target: lhs);
            edge?.SetLabel("Assign", node.OperatorToken.GetLocation());
            return lhs;
        }

        public override TypeWithNode VisitBinaryExpression(BinaryExpressionSyntax node)
        {
            var lhs = Visit(node.Left);
            var rhs = Visit(node.Right);
            switch (node.Kind()) {
                case SyntaxKind.CoalesceExpression:
                    // TODO: handle generics (IEnumerable<string?>? ?? IEnumerable<string>) -> IEnumerable<string?>
                    return rhs;
                default:
                    throw new NotImplementedException(node.Kind().ToString());
            }
        }

        public override TypeWithNode VisitThisExpression(ThisExpressionSyntax node)
        {
            var typeInfo = semanticModel.GetTypeInfo(node, cancellationToken);
            return new TypeWithNode(typeInfo.Type, typeSystem.NonNullNode);
        }

        private TypeWithNode currentMethodReturnType;

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
                node.Body?.Accept(this);
                if (node.ExpressionBody != null) {
                    var returnType = Visit(node.ExpressionBody.Expression);
                    var edge = CreateAssignmentEdge(source: returnType, target: currentMethodReturnType);
                    edge?.SetLabel("return", node.ExpressionBody.GetLocation());
                }
            } finally {
                currentMethodReturnType = outerMethodReturnType;
            }
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitReturnStatement(ReturnStatementSyntax node)
        {
            if (node.Expression != null) {
                var returnType = Visit(node.Expression);
                var edge = CreateAssignmentEdge(source: returnType, target: currentMethodReturnType);
                edge?.SetLabel("return", node.GetLocation());
            }
            return typeSystem.VoidType;
        }

        private NullabilityEdge? CreateAssignmentEdge(TypeWithNode source, TypeWithNode target)
        {
            // TODO: generics
            return CreateEdge(source.Node, target.Node);
        }

        private void Dereference(TypeWithNode type, in SyntaxToken derefToken)
        {
            var edge = CreateEdge(type.Node, typeSystem.NonNullNode);
            edge?.SetLabel("Deref", derefToken.GetLocation());
        }

        /// <summary>
        /// Creates an edge source->target.
        /// </summary>
        private NullabilityEdge? CreateEdge(NullabilityNode source, NullabilityNode target)
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
