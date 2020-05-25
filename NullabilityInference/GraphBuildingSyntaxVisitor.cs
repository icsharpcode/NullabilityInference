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
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace ICSharpCode.NullabilityInference
{
    /// <summary>
    /// Base class for NodeBuildingSyntaxVisitor and EdgeBuildingSyntaxVisitor.
    /// </summary>
    internal abstract class GraphBuildingSyntaxVisitor : CSharpSyntaxVisitor<TypeWithNode>
    {
        internal readonly SemanticModel semanticModel;
        protected readonly TypeSystem.Builder typeSystem;
        protected readonly CancellationToken cancellationToken;

        protected GraphBuildingSyntaxVisitor(SemanticModel semanticModel, TypeSystem.Builder typeSystem, CancellationToken cancellationToken)
        {
            this.semanticModel = semanticModel;
            this.typeSystem = typeSystem;
            this.cancellationToken = cancellationToken;
        }

        public override TypeWithNode VisitIdentifierName(IdentifierNameSyntax node)
        {
            return HandleTypeName(node, null);
        }

        public override TypeWithNode VisitGenericName(GenericNameSyntax node)
        {
            return HandleTypeName(node, node.TypeArgumentList.Arguments);
        }

        public override TypeWithNode VisitPredefinedType(PredefinedTypeSyntax node)
        {
            return HandleTypeName(node, null);
        }

        public override TypeWithNode VisitQualifiedName(QualifiedNameSyntax node)
        {
            return HandleTypeName(node, CollectTypeArgs(node));
        }

        protected List<TypeSyntax> CollectTypeArgs(ExpressionSyntax node)
        { 
            List<TypeSyntax> typeArgs = new List<TypeSyntax>();
            Visit(node);
            return typeArgs;

            void Visit(ExpressionSyntax s)
            {
                switch (s) {
                    case MemberAccessExpressionSyntax maes:
                        Visit(maes.Expression);
                        Visit(maes.Name);
                        break;
                    case QualifiedNameSyntax qns:
                        Visit(qns.Left);
                        Visit(qns.Right);
                        break;
                    case AliasQualifiedNameSyntax aqns:
                        Visit(aqns.Name);
                        break;
                    case GenericNameSyntax gns:
                        typeArgs.AddRange(gns.TypeArgumentList.Arguments);
                        break;
                }
            }
        }

        public override TypeWithNode VisitAliasQualifiedName(AliasQualifiedNameSyntax node)
        {
            return HandleTypeName(node, (node.Name as GenericNameSyntax)?.TypeArgumentList.Arguments);
        }

        protected abstract TypeWithNode HandleTypeName(TypeSyntax node, IEnumerable<TypeSyntax>? typeArguments);

        protected TypeWithNode[] InheritOuterTypeArguments(TypeWithNode[]? syntacticTypeArgs, ITypeSymbol ty)
        {
            if (ty.ContainingType.FullArity() > 0) {
                var typeArgs = new TypeWithNode[ty.FullArity()];
                int pos = typeArgs.Length;
                if (syntacticTypeArgs != null) {
                    pos -= syntacticTypeArgs.Length;
                    Array.Copy(syntacticTypeArgs, 0, typeArgs, pos, syntacticTypeArgs.Length);
                }
                var outerTypeParameters = ty.ContainingType.FullTypeArguments().ToList();
                for (pos--; pos >= 0; pos--) {
                    typeArgs[pos] = new TypeWithNode(outerTypeParameters[pos], typeSystem.ObliviousNode);
                }
                return typeArgs;
            } else {
                return syntacticTypeArgs ?? new TypeWithNode[0];
            }
        }

        public override TypeWithNode VisitNullableType(NullableTypeSyntax node)
        {
            var ty = node.ElementType.Accept(this);
            if (ty.Type?.IsValueType == true) {
                var symbolInfo = semanticModel.GetSymbolInfo(node, cancellationToken);
                if (symbolInfo.Symbol is INamedTypeSymbol { OriginalDefinition: { SpecialType: SpecialType.System_Nullable_T } } nullableType) {
                    return new TypeWithNode(nullableType, typeSystem.ObliviousNode, new[] { ty });
                } else {
                    throw new NotSupportedException("NullableType should resolve to System_Nullable_T");
                }
            } else {
                // Ignore existing nullable reference types; we'll infer them again from scratch.
                return ty;
            }
        }

        /// <summary>
        /// Gets whether it is syntactically possible to add a `NullableTypeSyntax` around the given node.
        /// </summary>
        internal static bool CanBeMadeNullableSyntax(TypeSyntax node)
        {
            if (node is IdentifierNameSyntax { IsVar: true })
                return false;
            if (node.Parent is TypeParameterConstraintClauseSyntax constraint && constraint.Name == node)
                return false;
            switch (node.Parent?.Kind()) {
                case SyntaxKind.ExplicitInterfaceSpecifier:
                case SyntaxKind.TypeOfExpression:
                case SyntaxKind.IsExpression:
                case SyntaxKind.ObjectCreationExpression:
                case SyntaxKind.ArrayCreationExpression:
                case SyntaxKind.QualifiedName:
                case SyntaxKind.SimpleMemberAccessExpression:
                case SyntaxKind.SimpleBaseType:
                case SyntaxKind.AsExpression:
                    return false;
            }
            return true;
        }
    }
}
