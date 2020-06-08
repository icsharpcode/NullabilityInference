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
                var typeInfo = semanticModel.GetTypeInfo(node, cancellationToken);
                if (typeInfo.Type is INamedTypeSymbol { OriginalDefinition: { SpecialType: SpecialType.System_Nullable_T } } nullableType) {
                    return new TypeWithNode(nullableType, typeSystem.ObliviousNode, new[] { ty });
                } else {
                    throw new NotSupportedException("NullableType should resolve to System_Nullable_T");
                }
            } else {
                // Ignore existing nullable reference types; we'll infer them again from scratch.
                return ty;
            }
        }

        protected abstract NullabilityNode GetMappedNode(TypeSyntax node);

        public override TypeWithNode VisitArrayType(ArrayTypeSyntax node)
        {
            var type = node.ElementType.Accept(this);
            // Handle nested arrays
            foreach (var rankSpec in node.RankSpecifiers.Reverse()) {
                // Trying to insert `?` for nested arrays will be tricky,
                // because `int[,][]` with nullable nested arrays has to turn into `int[]?[,]`
                // So for now, just handle nested arrays as oblivious.
                var arrayType = type.Type != null ? semanticModel.Compilation.CreateArrayTypeSymbol(type.Type, rankSpec.Rank) : null;
                type = new TypeWithNode(arrayType, typeSystem.ObliviousNode, new[] { type });
            }
            if (CanBeMadeNullableSyntax(node))
                return type.WithNode(GetMappedNode(node));
            else
                return type;
        }

        public override TypeWithNode VisitPointerType(PointerTypeSyntax node)
        {
            var typeInfo = semanticModel.GetTypeInfo(node, cancellationToken);
            Debug.Assert(typeInfo.Type is IPointerTypeSymbol);
            var elementType = node.ElementType.Accept(this);
            return new TypeWithNode(typeInfo.Type, typeSystem.ObliviousNode, new[] { elementType });
        }

        public override TypeWithNode VisitTupleType(TupleTypeSyntax node)
        {
            var elementTypes = node.Elements.Select(e => e.Type.Accept(this)).ToArray();
            var symbolInfo = semanticModel.GetSymbolInfo(node, cancellationToken);
            return new TypeWithNode(symbolInfo.Symbol as ITypeSymbol, typeSystem.ObliviousNode, elementTypes);
        }

        public override TypeWithNode VisitRefType(RefTypeSyntax node)
        {
            return node.Type.Accept(this);
        }

        public override TypeWithNode VisitOmittedTypeArgument(OmittedTypeArgumentSyntax node)
        {
            // e.g. in `typeof(List<>)`
            return typeSystem.VoidType;
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
                case SyntaxKind.ExplicitInterfaceSpecifier: // void IDisposable?.Dispose()
                case SyntaxKind.TypeOfExpression: // typeof(string?)
                case SyntaxKind.IsExpression: // x is string?
                case SyntaxKind.ObjectCreationExpression: // new Class?()
                case SyntaxKind.ArrayCreationExpression:  // new string[]?
                case SyntaxKind.QualifiedName:
                case SyntaxKind.AliasQualifiedName:
                case SyntaxKind.SimpleMemberAccessExpression: // Console?.WriteLine
                case SyntaxKind.SimpleBaseType: // : IDisposable?
                case SyntaxKind.AsExpression: // x as string?
                case SyntaxKind.UsingDirective: // using System?;
                case SyntaxKind.DeclarationPattern: // x is string? b
                case SyntaxKind.Argument: // type name can appears as argument in nameof() expression
                    return false;
            }
            return true;
        }
    }
}
