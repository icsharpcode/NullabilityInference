// Copyright (c) 2020 Daniel Grunwald

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace NullabilityInference
{
    /// <summary>
    /// A node representing either a fixed 
    /// </summary>
    public abstract class NullabilityNode
    {
        /// <summary>
        /// List of incoming edges.
        /// </summary>
        internal List<NullabilityEdge> IncomingEdges = new List<NullabilityEdge>();
        /// <summary>
        /// List of outgoing edges.
        /// </summary>
        internal List<NullabilityEdge> OutgoingEdges = new List<NullabilityEdge>();

        public IEnumerable<NullabilityNode> Predecessors => IncomingEdges.Select(e => e.Source);
        public IEnumerable<NullabilityNode> Successors => OutgoingEdges.Select(e => e.Target);

        /// <summary>
        /// Name for the DOT graph.
        /// </summary>
        public abstract string Name { get; }

        /// <summary>
        /// Location in the source code for this node.
        /// </summary>
        public abstract Location? Location { get; }

        internal bool Visited;

        public NullType NullType = NullType.Infer;

        internal virtual void SetName(string name)
        {
        }
    }

    [DebuggerDisplay("{Name}")]
    internal sealed class SpecialNullabilityNode : NullabilityNode
    {
        public SpecialNullabilityNode(NullType nullType)
        {
            this.NullType = nullType;
        }

        public override Location? Location => null;

        public override string Name => NullType switch
        {
            NullType.Nullable => "<nullable>",
            NullType.NonNull => "<nonnull>",
            NullType.Oblivious => "<oblivious>",
            _ => throw new NotSupportedException(),
        };
    }

    [DebuggerDisplay("{Name}")]
    internal sealed class SyntacticNullabilityNode : NullabilityNode
    {
        private readonly TypeSyntax typeSyntax;
        private readonly int id;

        public SyntacticNullabilityNode(TypeSyntax typeSyntax, int id)
        {
            this.typeSyntax = typeSyntax;
            this.id = id;
        }

        public override Location? Location => typeSyntax.GetLocation();

        private string? symbolName;

        public override string Name => $"{symbolName}#{id}";

        internal override void SetName(string name)
        {
            symbolName ??= name;
        }
    }

    internal sealed class TemporaryNullabilityNode : NullabilityNode
    {
        private static long nextId;
        private readonly long id = Interlocked.Increment(ref nextId);

        public override Location? Location => null;
        
        private string symbolName = "temp";

        public override string Name => $"<{symbolName}#{id}>";

        internal override void SetName(string name)
        {
            if (symbolName == "temp")
                symbolName = name;
        }
    }
}
