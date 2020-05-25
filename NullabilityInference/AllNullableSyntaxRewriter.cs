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

using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace ICSharpCode.NullabilityInference
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
            if (!CanBeMadeNullableSyntax(node)) {
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

        public override SyntaxNode? VisitArrayType(ArrayTypeSyntax node)
        {
            var newNode = base.VisitArrayType(node);
            if (CanBeMadeNullableSyntax(node) && newNode is TypeSyntax newTypeSyntax) {
                return SyntaxFactory.NullableType(
                    elementType: newTypeSyntax.WithoutTrailingTrivia(),
                    questionToken: SyntaxFactory.Token(SyntaxKind.QuestionToken)
                ).WithTrailingTrivia(newTypeSyntax.GetTrailingTrivia());
            } else {
                return newNode;
            }
        }

        private bool CanBeMadeNullableSyntax(TypeSyntax node)
        {
            return GraphBuildingSyntaxVisitor.CanBeMadeNullableSyntax(node) && !(node.Parent is NullableTypeSyntax);
        }
    }
}
