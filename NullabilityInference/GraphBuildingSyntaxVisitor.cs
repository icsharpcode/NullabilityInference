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
            List<TypeSyntax> typeArgs = new List<TypeSyntax>();
            CollectTypeArgs(node.Left);
            CollectTypeArgs(node.Right);
            return HandleTypeName(node, typeArgs);

            void CollectTypeArgs(TypeSyntax s)
            {
                switch (s) {
                    case QualifiedNameSyntax qns:
                        CollectTypeArgs(qns.Left);
                        CollectTypeArgs(qns.Right);
                        break;
                    case AliasQualifiedNameSyntax aqns:
                        CollectTypeArgs(aqns.Name);
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
            if (node.Parent is ObjectCreationExpressionSyntax)
                return false;
            if (node.Parent is QualifiedNameSyntax)
                return false;
            return true;
        }
    }
}
