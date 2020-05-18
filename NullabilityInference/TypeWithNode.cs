// Copyright (c) 2020 Daniel Grunwald

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using Microsoft.CodeAnalysis;

namespace NullabilityInference
{
    /// <summary>
    /// Pairs a C# type with a nullability node that can be used to infer the nullability.
    /// </summary>
    [DebuggerDisplay("{Type}{Node}")]
    public readonly struct TypeWithNode
    {
        public readonly ITypeSymbol? Type;
        public readonly NullabilityNode Node;
        // This should be ImmutableArray<TypeWithNode>, but that runs into
        //   https://github.com/dotnet/runtime/issues/12024
        public readonly IReadOnlyList<TypeWithNode> TypeArguments;
        private static readonly TypeWithNode[] emptyTypeArguments = { };

        public TypeWithNode(ITypeSymbol? type, NullabilityNode node, IReadOnlyList<TypeWithNode>? typeArguments = null)
        {
            this.Type = type;
            this.Node = node;
            this.TypeArguments = typeArguments ?? emptyTypeArguments;
            if (type is INamedTypeSymbol nt) {
                Debug.Assert(nt.Arity == this.TypeArguments.Count);
            } else if (type is IArrayTypeSymbol || type is IPointerTypeSymbol) {
                Debug.Assert(this.TypeArguments.Count == 1);
            } else {
                Debug.Assert(this.TypeArguments.Count == 0);
            }
        }

        /// <summary>
        /// Replaces the top-level nullability.
        /// </summary>
        internal TypeWithNode WithNode(NullabilityNode newNode)
        {
            return new TypeWithNode(Type, newNode, TypeArguments);
        }

        /// <summary>
        /// Creates a new TypeWithNode
        /// 
        /// newType must be the result of applying the substitution to this.Type.
        /// </summary>
        internal TypeWithNode WithSubstitution(ITypeSymbol newType, TypeSubstitution subst)
        {
            if (this.Type is ITypeParameterSymbol tp) {
                var substituted = subst[tp.TypeParameterKind, tp.Ordinal];
                Debug.Assert(SymbolEqualityComparer.Default.Equals(substituted.Type, newType));
                return substituted;
            } else if (this.Type is INamedTypeSymbol thisNamedTypeSymbol && newType is INamedTypeSymbol newNamedTypeSymbol) {
                Debug.Assert(SymbolEqualityComparer.Default.Equals(thisNamedTypeSymbol.OriginalDefinition, newNamedTypeSymbol.OriginalDefinition));
                Debug.Assert(newNamedTypeSymbol.Arity == this.TypeArguments.Count);
                TypeWithNode[] newTypeArgs = new TypeWithNode[this.TypeArguments.Count];
                for (int i = 0; i < newTypeArgs.Length; i++) {
                    newTypeArgs[i] = this.TypeArguments[i].WithSubstitution(newNamedTypeSymbol.TypeArguments[i], subst);
                }
                return new TypeWithNode(newType, this.Node, newTypeArgs);
            } else if (this.Type is IArrayTypeSymbol thisArrayTypeSymbol && newType is IArrayTypeSymbol newArrayTypeSymbol) {
                Debug.Assert(thisArrayTypeSymbol.Rank == newArrayTypeSymbol.Rank);
                var elementType = this.TypeArguments.Single().WithSubstitution(newArrayTypeSymbol.ElementType, subst);
                return new TypeWithNode(newType, this.Node, new[] { elementType });
            } else if (this.Type is IPointerTypeSymbol && newType is IPointerTypeSymbol newPointerTypeSymbol) {
                var pointedAtType = this.TypeArguments.Single().WithSubstitution(newPointerTypeSymbol.PointedAtType, subst);
                return new TypeWithNode(newType, this.Node, new[] { pointedAtType });
            }
            return new TypeWithNode(newType, this.Node);
        }

        [Conditional("DEBUG")]
        internal void SetName(string name)
        {
            Node.SetName(name);
            for (int i = 0; i < TypeArguments.Count; i++) {
                TypeArguments[i].SetName($"{name}!{i}");
            }
        }

        internal IEnumerable<(NullabilityNode, VarianceKind)> NodesWithVariance()
        {
            yield return (Node, VarianceKind.Out);
            if (this.Type is INamedTypeSymbol namedType) {
                Debug.Assert(this.TypeArguments.Count == namedType.TypeParameters.Length);
                foreach (var (tp, ta) in namedType.TypeParameters.Zip(this.TypeArguments)) {
                    foreach (var (n, v) in ta.NodesWithVariance()) {
                        yield return (n, (v, tp.Variance).Combine());
                    }
                }
            } else if (this.Type is IArrayTypeSymbol || this.Type is IPointerTypeSymbol) {
                foreach (var pair in this.TypeArguments.Single().NodesWithVariance()) {
                    yield return pair;
                }
            } else {
                Debug.Assert(this.TypeArguments.Count == 0);
            }
        }
    }

    public readonly struct TypeSubstitution
    {
        public readonly IReadOnlyList<TypeWithNode> ClassTypeArguments;
        public readonly IReadOnlyList<TypeWithNode> MethodTypeArguments;

        public TypeSubstitution(IReadOnlyList<TypeWithNode> classTypeArguments, IReadOnlyList<TypeWithNode> methodTypeArguments)
        {
            this.ClassTypeArguments = classTypeArguments;
            this.MethodTypeArguments = methodTypeArguments;
        }

        public TypeWithNode this[TypeParameterKind kind, int i] => kind switch
        {
            TypeParameterKind.Type => ClassTypeArguments[i],
            TypeParameterKind.Method => MethodTypeArguments[i],
            _ => throw new NotSupportedException("Unknown TypeParameterKind")
        };
    }
}
