// Copyright (c) 2020 Daniel Grunwald

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace NullabilityInference
{
    /// <summary>
    /// Maps from syntax nodes to nullability nodes.
    /// There is one mapping per SyntaxTree.
    /// </summary>
    internal class SyntaxToNodeMapping
    {
        private readonly Dictionary<TypeSyntax, SyntacticNullabilityNode> nodes = new Dictionary<TypeSyntax, SyntacticNullabilityNode>();

        public SyntacticNullabilityNode this[TypeSyntax syntax] => nodes[syntax];

        public IEnumerable<SyntacticNullabilityNode> Nodes => nodes.Values;

        public SyntacticNullabilityNode CreateNewNode(TypeSyntax syntax)
        {
            var nullNode = new SyntacticNullabilityNode(syntax, nodes.Count + 1);
            nodes.Add(syntax, nullNode);
            return nullNode;
        }
    }

    /// <summary>
    /// Creates NullabilityNodes for types that appear in the syntax tree.
    /// 
    /// We create a NullabilityNode for each syntactic occurrence of a reference type,
    /// as those are the places where we could insert `?`.
    /// This visitor maps every `TypeSyntax` to a `TypeWithNode`.
    /// All other kinds of syntax nodes just return `typeSystem.VoidType` (even expressions that don't produce `void`
    /// -- only the later EdgeBuildingSyntaxVisitor uses accurate types for expressions).
    /// For declarations, this visitor registers the `TypeWithNode` with our `typeSystem` instance.
    /// </summary>
    internal sealed class NodeBuildingSyntaxVisitor : GraphBuildingSyntaxVisitor
    {
        private readonly SemanticModel semanticModel;
        private readonly TypeSystem.Builder typeSystem;
        private readonly CancellationToken cancellationToken;

        /// <summary>
        /// Gets the resulting SyntaxToNodeMapping.
        /// </summary>
        public SyntaxToNodeMapping Mapping { get; } = new SyntaxToNodeMapping();

        public NodeBuildingSyntaxVisitor(SemanticModel semanticModel, TypeSystem.Builder typeSystem, CancellationToken cancellationToken)
        {
            this.semanticModel = semanticModel;
            this.typeSystem = typeSystem;
            this.cancellationToken = cancellationToken;
        }

        public override TypeWithNode DefaultVisit(SyntaxNode node)
        {
            cancellationToken.ThrowIfCancellationRequested();
            Debug.Assert(!(node is TypeSyntax));
            foreach (var child in node.ChildNodes()) {
                Visit(child);
            }
            return typeSystem.VoidType;
        }

        protected override TypeWithNode HandleTypeName(TypeSyntax node, IEnumerable<TypeSyntax>? typeArguments)
        {
            TypeWithNode[]? typeArgs = typeArguments?.Select(s => s.Accept(this)).ToArray();
            var symbolInfo = semanticModel.GetSymbolInfo(node, cancellationToken);
            if (symbolInfo.Symbol is ITypeSymbol ty) {
                if (ty.IsReferenceType && CanBeMadeNullableSyntax(node)) {
                    return new TypeWithNode(ty, Mapping.CreateNewNode(node), typeArgs);
                } else {
                    return new TypeWithNode(ty, typeSystem.ObliviousNode, typeArgs);
                }
            }
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitArrayType(ArrayTypeSyntax node)
        {
            var elementType = node.ElementType.Accept(this);
            var arrayType = elementType.Type != null ? semanticModel.Compilation.CreateArrayTypeSymbol(elementType.Type) : null;
            // in an ArrayCreationExpression, the rank specifiers may contain arbitrary sub-expressions
            foreach (var rank in node.RankSpecifiers) {
                rank.Accept(this);
            }
            var nullNode = CanBeMadeNullableSyntax(node) ? Mapping.CreateNewNode(node) : typeSystem.ObliviousNode;
            return new TypeWithNode(arrayType, nullNode, new[] { elementType });
        }

        public override TypeWithNode VisitVariableDeclaration(VariableDeclarationSyntax node)
        {
            foreach (var v in node.Variables)
                v.Accept(this);
            if (!(node.Type is SimpleNameSyntax { IsVar: true })) {
                var type = node.Type.Accept(this);
                foreach (var v in node.Variables) {
                    var symbol = semanticModel.GetDeclaredSymbol(v, cancellationToken);
                    if (symbol != null) {
                        typeSystem.AddSymbolType(symbol, type);
                    }
                }
            }
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitParameter(ParameterSyntax node)
        {
            if (node.Type != null) {
                var type = node.Type.Accept(this);
                var symbol = semanticModel.GetDeclaredSymbol(node, cancellationToken);
                if (symbol != null) {
                    parameterTypes.Add(symbol, type);
                    typeSystem.AddSymbolType(symbol, type);
                }
            }
            node.Default?.Accept(this);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitConstructorDeclaration(ConstructorDeclarationSyntax node)
        {
            return HandleMember(node, null);
        }

        public override TypeWithNode VisitMethodDeclaration(MethodDeclarationSyntax node)
        {
            return HandleMember(node, node.ReturnType);
        }

        public override TypeWithNode VisitOperatorDeclaration(OperatorDeclarationSyntax node)
        {
            return HandleMember(node, node.ReturnType);
        }

        public override TypeWithNode VisitConversionOperatorDeclaration(ConversionOperatorDeclarationSyntax node)
        {
            return HandleMember(node, node.Type);
        }

        public override TypeWithNode VisitPropertyDeclaration(PropertyDeclarationSyntax node)
        {
            return HandleMember(node, node.Type);
        }

        public override TypeWithNode VisitIndexerDeclaration(IndexerDeclarationSyntax node)
        {
            return HandleMember(node, node.Type);
        }

        private ISymbol? currentMember;

        private TypeWithNode HandleMember(MemberDeclarationSyntax node, TypeSyntax? typeSyntax)
        {
            var outerMember = currentMember;
            try {
                currentMember = semanticModel.GetDeclaredSymbol(node, cancellationToken);
                if (typeSyntax != null) {
                    var returnType = typeSyntax.Accept(this);
                    if (currentMember != null) {
                        typeSystem.AddSymbolType(currentMember, returnType);
                    }
                }
                foreach (var child in node.ChildNodes()) {
                    if (child != typeSyntax)
                        Visit(child);
                }
                return typeSystem.VoidType;
            } finally {
                currentMember = outerMember;
            }
        }

        public override TypeWithNode VisitObjectCreationExpression(ObjectCreationExpressionSyntax node)
        {
            var typeNode = node.Type.Accept(this);
            typeNode.SetName("new");
            node.ArgumentList?.Accept(this);
            node.Initializer?.Accept(this);
            return typeNode;
        }

        public override TypeWithNode VisitCastExpression(CastExpressionSyntax node)
        {
            var typeNode = node.Type.Accept(this);
            typeNode.SetName("cast");
            node.Expression.Accept(this);
            return typeNode;
        }

        public override TypeWithNode VisitThrowExpression(ThrowExpressionSyntax node)
        {
            HandleThrow(node.Expression);
            return base.VisitThrowExpression(node);
        }

        public override TypeWithNode VisitThrowStatement(ThrowStatementSyntax node)
        {
            HandleThrow(node.Expression);
            return base.VisitThrowStatement(node);
        }

        private readonly Dictionary<IParameterSymbol, TypeWithNode> parameterTypes = new Dictionary<IParameterSymbol, TypeWithNode>();

        private void HandleThrow(ExpressionSyntax? exceptionSyntax)
        {
            // Treat `throw ArgumentNullException(paramName)` as a hint that `paramName` is non-nullable.
            // Without this special case, we will likely end up inferring the parameter as nullable,
            // since there's a (throwing) code path handling `null`, so flow analysis prevents
            // us from creating edges for uses of the parameter.
            if (exceptionSyntax is ObjectCreationExpressionSyntax oce) {
                if (IsArgumentNullException(oce.Type) && oce.ArgumentList?.Arguments.Count == 1 && IsParameterName(oce.ArgumentList.Arguments.Single(), out var param)) {
                    var paramType = parameterTypes[param];
                    paramType.Node.ReplaceWith(typeSystem.NonNullNode);
                }
            }

            bool IsArgumentNullException(TypeSyntax type)
            {
                var symbolInfo = semanticModel.GetSymbolInfo(type, cancellationToken);
                return symbolInfo.Symbol is INamedTypeSymbol { Name: "ArgumentNullException" };
            }
        }


        private bool IsParameterName(ArgumentSyntax? argument, [NotNullWhen(returnValue: true)] out IParameterSymbol? parameter)
        {
            parameter = null;
            if (argument == null)
                return false;
            if (argument.Expression is LiteralExpressionSyntax literal && literal.Kind() == SyntaxKind.StringLiteralExpression) {
                string name = literal.Token.ValueText;
               if (currentMember is IMethodSymbol method) {
                    parameter = method.Parameters.SingleOrDefault(p => p.Name == name);
                } else if (currentMember is IPropertySymbol property) {
                    parameter = property.Parameters.SingleOrDefault(p => p.Name == name);
                }
                return parameter != null;
            } else if (argument.Expression is InvocationExpressionSyntax
            {
                Expression: IdentifierNameSyntax { Identifier: { Text: "nameof" } },
                ArgumentList: { Arguments: { Count: 1 } nameofArgs }
            }) {
                var symbolInfo = semanticModel.GetSymbolInfo(nameofArgs.Single().Expression, cancellationToken);
                parameter = symbolInfo.Symbol as IParameterSymbol;
                return parameter != null;
            } else {
                return false;
            }
        }
    }
}
