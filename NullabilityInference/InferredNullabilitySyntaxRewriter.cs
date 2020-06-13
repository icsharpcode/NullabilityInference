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

using System.Diagnostics;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace ICSharpCode.NullabilityInference
{
    /// <summary>
    /// Rewrites a C# syntax tree by replacing nullable reference type syntax with that inferred by our analysis.
    /// </summary>
    internal sealed class InferredNullabilitySyntaxRewriter : CSharpSyntaxRewriter
    {
        private readonly SemanticModel semanticModel;
        private readonly TypeSystem typeSystem;
        private readonly SyntaxToNodeMapping mapping;
        private readonly CancellationToken cancellationToken;

        public InferredNullabilitySyntaxRewriter(SemanticModel semanticModel, TypeSystem typeSystem, SyntaxToNodeMapping mapping, CancellationToken cancellationToken)
            : base(visitIntoStructuredTrivia: true)
        {
            this.semanticModel = semanticModel;
            this.typeSystem = typeSystem;
            this.mapping = mapping;
            this.cancellationToken = cancellationToken;
        }

        private bool isActive = true;

        public override SyntaxNode? VisitNullableDirectiveTrivia(NullableDirectiveTriviaSyntax node)
        {
            isActive = node.SettingToken.IsKind(SyntaxKind.RestoreKeyword);
            return base.VisitNullableDirectiveTrivia(node);
        }

        public override SyntaxNode? VisitNullableType(NullableTypeSyntax node)
        {
            var elementType = node.ElementType.Accept(this);
            if (elementType == null)
                return null;
            var symbolInfo = semanticModel.GetSymbolInfo(node);
            if (isActive && symbolInfo.Symbol is ITypeSymbol { IsReferenceType: true }) {
                // Remove existing nullable reference types
                return elementType.WithTrailingTrivia(node.GetTrailingTrivia());
            } else {
                return node.ReplaceNode(node.ElementType, elementType);
            }
        }

        public override SyntaxNode? VisitIdentifierName(IdentifierNameSyntax node)
        {
            return HandleTypeName(node, base.VisitIdentifierName(node));
        }

        public override SyntaxNode? VisitGenericName(GenericNameSyntax node)
        {
            return HandleTypeName(node, base.VisitGenericName(node));
        }

        public override SyntaxNode? VisitPredefinedType(PredefinedTypeSyntax node)
        {
            return HandleTypeName(node, base.VisitPredefinedType(node));
        }

        public override SyntaxNode? VisitQualifiedName(QualifiedNameSyntax node)
        {
            return HandleTypeName(node, base.VisitQualifiedName(node));
        }

        private SyntaxNode? HandleTypeName(TypeSyntax node, SyntaxNode? newNode)
        {
            if (!isActive || !GraphBuildingSyntaxVisitor.CanBeMadeNullableSyntax(node)) {
                return newNode;
            }
            var symbolInfo = semanticModel.GetSymbolInfo(node, cancellationToken);
            if (symbolInfo.Symbol is ITypeSymbol ty && ty.CanBeMadeNullable() && newNode is TypeSyntax newTypeSyntax) {
                var nullNode = mapping[node];
                if (nullNode.NullType == NullType.Nullable) {
                    return SyntaxFactory.NullableType(
                        elementType: newTypeSyntax.WithoutTrailingTrivia(),
                        questionToken: SyntaxFactory.Token(SyntaxKind.QuestionToken)
                    ).WithTrailingTrivia(newTypeSyntax.GetTrailingTrivia());
                }
            }
            return newNode;
        }

        public override SyntaxNode? VisitArrayType(ArrayTypeSyntax node)
        {
            var newNode = base.VisitArrayType(node);
            if (isActive && GraphBuildingSyntaxVisitor.CanBeMadeNullableSyntax(node) && newNode is TypeSyntax newTypeSyntax) {
                var nullNode = mapping[node];
                if (nullNode.NullType == NullType.Nullable) {
                    return SyntaxFactory.NullableType(
                        elementType: newTypeSyntax.WithoutTrailingTrivia(),
                        questionToken: SyntaxFactory.Token(SyntaxKind.QuestionToken)
                    ).WithTrailingTrivia(newTypeSyntax.GetTrailingTrivia());
                }
            }
            return newNode;
        }

        public override SyntaxNode? VisitParameter(ParameterSyntax node)
        {
            var param = semanticModel.GetDeclaredSymbol(node, cancellationToken);
            node = (ParameterSyntax)base.VisitParameter(node)!;
            if (isActive && param != null && typeSystem.TryGetOutParameterFlowNodes(param, out var pair)) {
                if (pair.whenTrue.NullType != pair.whenFalse.NullType) {
                    Debug.Assert(pair.whenTrue.NullType == NullType.NonNull || pair.whenFalse.NullType == NullType.NonNull);
                    // Create [NotNullWhen] attribute
                    bool notNullWhen = (pair.whenTrue.NullType == NullType.NonNull);
                    var attrArgument = SyntaxFactory.LiteralExpression(notNullWhen ? SyntaxKind.TrueLiteralExpression : SyntaxKind.FalseLiteralExpression);
                    var newAttribute = SyntaxFactory.Attribute(SyntaxFactory.IdentifierName("NotNullWhen"),
                        SyntaxFactory.AttributeArgumentList(SyntaxFactory.SingletonSeparatedList(SyntaxFactory.AttributeArgument(attrArgument))));
                    var newAttributeList = SyntaxFactory.AttributeList(SyntaxFactory.SingletonSeparatedList(newAttribute));
                    node = node.AddAttributeLists(newAttributeList.WithTrailingTrivia(SyntaxFactory.Space));
                }
            }
            return node;
        }
    }
}
