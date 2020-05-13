// Copyright (c) 2020 Daniel Grunwald

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace NullabilityInference
{
    /// <summary>
    /// A C# syntax rewriter that marks all reference types as nullable.
    /// 
    /// This is used initially to construct the compilation that gets analyzed by the inference logic.
    /// Using nullable types everywhere makes it so that we can interpret NullableFlowState.NotNull
    /// as expression being protected by an `if (x != null)`.
    /// </summary>
    public sealed class AllNullableSyntaxRewriter : CSharpSyntaxRewriter
    {
        public static CSharpCompilation MakeAllReferenceTypesNullable(CSharpCompilation compilation, CancellationToken cancellationToken)
        {
            var newSyntaxTrees = compilation.SyntaxTrees.AsParallel().WithCancellation(cancellationToken)
                .Select(syntaxTree => {
                    var semanticModel = compilation.GetSemanticModel(syntaxTree);
                    var oldRoot = syntaxTree.GetRoot(cancellationToken);
                    var newRoot = new AllNullableSyntaxRewriter(semanticModel, cancellationToken).Visit(oldRoot);
                    return syntaxTree.WithRootAndOptions(newRoot, syntaxTree.Options);
                });
            return compilation.RemoveAllSyntaxTrees()
                .WithOptions(compilation.Options.WithNullableContextOptions(NullableContextOptions.Enable))
                .AddSyntaxTrees(newSyntaxTrees);
        }

        private readonly SemanticModel semanticModel;
        private readonly CancellationToken cancellationToken;

        private AllNullableSyntaxRewriter(SemanticModel semanticModel, CancellationToken cancellationToken)
        {
            this.semanticModel = semanticModel;
            this.cancellationToken = cancellationToken;
        }

        public override SyntaxNode? VisitIdentifierName(IdentifierNameSyntax node)
        {
            if (node.IsVar) {
                // can't use `var?`
                return base.VisitIdentifierName(node);
            }
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
            if (!NodeBuildingSyntaxVisitor.CanBeMadeNullableSyntax(node)) {
                return newNode;
            }
            var symbolInfo = semanticModel.GetSymbolInfo(node, cancellationToken);
            if (symbolInfo.Symbol is ITypeSymbol { IsReferenceType: true } && newNode is TypeSyntax newTypeSyntax) {
                return SyntaxFactory.NullableType(
                    elementType: newTypeSyntax.WithoutTrailingTrivia(),
                    questionToken: SyntaxFactory.Token(SyntaxKind.QuestionToken)
                ).WithTrailingTrivia(newTypeSyntax.GetTrailingTrivia());
            }
            return newNode;
        }
    }
}
