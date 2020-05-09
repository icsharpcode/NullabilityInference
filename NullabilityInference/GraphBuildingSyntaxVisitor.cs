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
        public override TypeWithNode VisitQualifiedName(QualifiedNameSyntax node)
        {
            node.Left.Accept(this);
            return node.Right.Accept(this);
        }

        public override TypeWithNode VisitAliasQualifiedName(AliasQualifiedNameSyntax node)
        {
            node.Alias.Accept(this);
            return node.Name.Accept(this);
        }

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
    }
}
