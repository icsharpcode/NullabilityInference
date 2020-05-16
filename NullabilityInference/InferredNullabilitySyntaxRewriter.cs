// Copyright (c) 2020 Daniel Grunwald

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
            if (!GraphBuildingSyntaxVisitor.CanBeMadeNullableSyntax(node)) {
                return newNode;
            }
            var symbolInfo = semanticModel.GetSymbolInfo(node, cancellationToken);
            if (symbolInfo.Symbol is ITypeSymbol { IsReferenceType: true } && newNode is TypeSyntax newTypeSyntax) {
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
            if (GraphBuildingSyntaxVisitor.CanBeMadeNullableSyntax(node) && newNode is TypeSyntax newTypeSyntax) {
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
