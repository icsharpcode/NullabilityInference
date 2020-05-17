// Copyright (c) 2020 Daniel Grunwald

using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace NullabilityInference
{
    /// <summary>
    /// Base class for NodeBuildingSyntaxVisitor and EdgeBuildingSyntaxVisitor.
    /// </summary>
    internal abstract class GraphBuildingSyntaxVisitor : CSharpSyntaxVisitor<TypeWithNode>
    {
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

        public override TypeWithNode VisitNullableType(NullableTypeSyntax node)
        {
            var ty = node.ElementType.Accept(this);
            if (ty.Type?.IsValueType == true) {
                // TODO: wrap in `Nullable<T>`
                // note that T may be a generic struct, so the type may include useful NullabilityNodes that we need to preserve
                throw new NotImplementedException();
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
            if (node.Parent is ObjectCreationExpressionSyntax || node.Parent is ArrayCreationExpressionSyntax)
                return false;
            if (node.Parent is QualifiedNameSyntax || node.Parent is MemberAccessExpressionSyntax)
                return false;
            if (node.Parent is BaseTypeSyntax)
                return false;
            return true;
        }
    }
}
