using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace NullabilityInference
{
    /// <summary>
    /// Rewrites a C# syntax tree by replacing nullable reference type syntax with that inferred by our analysis.
    /// </summary>
    internal sealed class InferredNullabilitySyntaxRewriter : CSharpSyntaxRewriter
    {
        private readonly SemanticModel semanticModel;
        private readonly SyntaxToNodeMapping mapping;
        private readonly CancellationToken cancellationToken;

        public InferredNullabilitySyntaxRewriter(SemanticModel semanticModel, SyntaxToNodeMapping mapping, CancellationToken cancellationToken)
        {
            this.semanticModel = semanticModel;
            this.mapping = mapping;
            this.cancellationToken = cancellationToken;
        }

        public override SyntaxNode? VisitNullableType(NullableTypeSyntax node)
        {
            var symbolInfo = semanticModel.GetSymbolInfo(node);
            if (symbolInfo.Symbol is ITypeSymbol { IsValueType: false }) {
                // Remove existing nullable reference types
                return node.ElementType.Accept(this).WithTrailingTrivia(node.GetTrailingTrivia());
            } else {
                return node.ReplaceNode(node.ElementType, node.ElementType.Accept(this));
            }
        }

        public override SyntaxNode? VisitIdentifierName(IdentifierNameSyntax node)
        {
            return HandleTypeName(node, base.VisitIdentifierName(node));
        }

        public override SyntaxNode? VisitPredefinedType(PredefinedTypeSyntax node)
        {
            return HandleTypeName(node, base.VisitPredefinedType(node));
        }

        private SyntaxNode? HandleTypeName(TypeSyntax node, SyntaxNode? newNode)
        {
            var symbolInfo = semanticModel.GetSymbolInfo(node, cancellationToken);
            if (symbolInfo.Symbol is ITypeSymbol { IsValueType: false } && newNode is TypeSyntax newTypeSyntax) {
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
    }
}
