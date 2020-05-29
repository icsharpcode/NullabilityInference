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
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Operations;

namespace ICSharpCode.NullabilityInference
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
        /// <summary>
        /// Gets the resulting SyntaxToNodeMapping.
        /// </summary>
        public SyntaxToNodeMapping Mapping { get; } = new SyntaxToNodeMapping();

        public NodeBuildingSyntaxVisitor(SemanticModel semanticModel, TypeSystem.Builder typeSystem, CancellationToken cancellationToken)
            : base(semanticModel, typeSystem, cancellationToken)
        {
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

        private readonly Dictionary<IAliasSymbol, TypeWithNode> aliases = new Dictionary<IAliasSymbol, TypeWithNode>();

        public override TypeWithNode VisitUsingDirective(UsingDirectiveSyntax node)
        {
            if (node.Alias != null) {
                var type = node.Name.Accept(this);
                var alias = semanticModel.GetDeclaredSymbol(node, cancellationToken);
                if (alias != null) {
                    aliases.Add(alias, type);
                    typeSystem.AddSymbolType(alias, type);
                }
                return typeSystem.VoidType;
            } else {
                return base.VisitUsingDirective(node);
            }
        }

        protected override TypeWithNode HandleTypeName(TypeSyntax node, IEnumerable<TypeSyntax>? typeArguments)
        {
            TypeWithNode[]? typeArgs = typeArguments?.Select(s => s.Accept(this)).ToArray();
            var symbolInfo = semanticModel.GetSymbolInfo(node, cancellationToken);
            if (symbolInfo.Symbol is ITypeSymbol ty) {
                var alias = semanticModel.GetAliasInfo(node, cancellationToken);
                if (alias != null) {
                    typeArgs = aliases[alias].TypeArguments.ToArray();
                } else {
                    typeArgs = InheritOuterTypeArguments(typeArgs, ty);
                }
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

        public override TypeWithNode VisitTupleType(TupleTypeSyntax node)
        {
            var elementTypes = node.Elements.Select(e => e.Type.Accept(this)).ToArray();
            var symbolInfo = semanticModel.GetSymbolInfo(node, cancellationToken);
            return new TypeWithNode(symbolInfo.Symbol as ITypeSymbol, typeSystem.ObliviousNode, elementTypes);
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

        public override TypeWithNode VisitDeclarationExpression(DeclarationExpressionSyntax node)
        {
            if (node.Type is SimpleNameSyntax { IsVar: true }) {
                node.Designation.Accept(this);
            } else {
                var type = node.Type.Accept(this);
                if (node.Designation is SingleVariableDesignationSyntax svds) {
                    var symbol = semanticModel.GetDeclaredSymbol(svds, cancellationToken);
                    if (symbol != null) {
                        typeSystem.AddSymbolType(symbol, type);
                    }
                } else {
                    throw new NotImplementedException($"DeclarationExpression with explicit type unsupported designation: {node.Designation}");
                }
            }
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitSingleVariableDesignation(SingleVariableDesignationSyntax node)
        {
            var symbol = semanticModel.GetDeclaredSymbol(node, cancellationToken);
            if (symbol is ILocalSymbol local) {
                var type = typeSystem.CreateTemporaryType(local.Type);
                typeSystem.AddSymbolType(symbol, type);
                return type;
            } else {
                throw new NotSupportedException("SingleVariableDesignationSyntax should declare a LocalSymbol");
            }
        }

        public override TypeWithNode VisitCatchDeclaration(CatchDeclarationSyntax node)
        {
            var type = node.Type.Accept(this);
            var symbol = semanticModel.GetDeclaredSymbol(node, cancellationToken);
            if (symbol != null) {
                typeSystem.AddSymbolType(symbol, type);
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

        public override TypeWithNode VisitEnumMemberDeclaration(EnumMemberDeclarationSyntax node)
        {
            var symbol = semanticModel.GetDeclaredSymbol(node, cancellationToken);
            if (symbol != null) {
                typeSystem.AddSymbolType(symbol, new TypeWithNode(symbol.Type, typeSystem.ObliviousNode));
            }
            return base.VisitEnumMemberDeclaration(node);
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

        public override TypeWithNode VisitDelegateDeclaration(DelegateDeclarationSyntax node)
        {
            node.TypeParameterList?.Accept(this);
            node.ParameterList.Accept(this);
            var returnType = node.ReturnType.Accept(this);
            var delegateType = semanticModel.GetDeclaredSymbol(node, cancellationToken);
            if (delegateType?.DelegateInvokeMethod != null) {
                typeSystem.AddSymbolType(delegateType.DelegateInvokeMethod, returnType);
            }
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitSimpleBaseType(SimpleBaseTypeSyntax node)
        {
            var baseType = node.Type.Accept(this);
            var typeDecl = node.Ancestors().OfType<TypeDeclarationSyntax>().First();
            var currentType = semanticModel.GetDeclaredSymbol(typeDecl, cancellationToken);
            if (currentType is INamedTypeSymbol namedType) {
                typeSystem.AddBaseType(namedType, baseType);
            }
            return baseType;
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

        public override TypeWithNode VisitForEachStatement(ForEachStatementSyntax node)
        {
            TypeWithNode elementType;
            if (node.Type is SimpleNameSyntax { IsVar: true }) {
                var loopInfo = semanticModel.GetForEachStatementInfo(node);
                elementType = typeSystem.CreateTemporaryType(loopInfo.ElementType);
            } else {
                elementType = node.Type.Accept(this);
            }
            var loopVariable = semanticModel.GetDeclaredSymbol(node, cancellationToken);
            if (loopVariable != null) {
                typeSystem.AddSymbolType(loopVariable, elementType);
            }
            node.Expression.Accept(this);
            node.Statement.Accept(this);
            return typeSystem.VoidType;
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
