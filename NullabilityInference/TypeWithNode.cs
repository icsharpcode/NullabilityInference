// Copyright (c) 2020 Daniel Grunwald

using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
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
        public readonly IReadOnlyList<TypeWithNode> TypeArguments;
        private static readonly TypeWithNode[] emptyTypeArguments = { };

        public TypeWithNode(ITypeSymbol? type, NullabilityNode node, IReadOnlyList<TypeWithNode>? typeArguments = null)
        {
            this.Type = type;
            this.Node = node;
            this.TypeArguments = typeArguments ?? emptyTypeArguments;
            if (type is INamedTypeSymbol nt) {
                Debug.Assert(nt.Arity == this.TypeArguments.Count);
            }
        }

        /// <summary>
        /// Replaces the top-level nullability.
        /// </summary>
        internal TypeWithNode WithNode(NullabilityNode newNode)
        {
            return new TypeWithNode(Type, newNode, TypeArguments);
        }
    }
}
