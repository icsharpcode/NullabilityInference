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
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace ICSharpCode.NullabilityInference
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
        /// Predecessors in the residual graph. This starts out the same as <c>Predecessors</c>, but gets mutated by the maximum flow algorithm:
        /// Any edge involved in the maximum flow will be reversed. Thus there will be no path from {null} to {nonnull} left over in the residual graph.
        /// 
        /// Because we store only predecessors, this means a depth first search starting at {nonnull} using this list will visit all nodes
        /// on the {nonnull} half of the graph after performing the minimum cut.
        /// </summary>
        internal List<NullabilityNode> ResidualGraphPredecessors = new List<NullabilityNode>();

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

        private NullabilityNode? replacement;

        public NullabilityNode ReplacedWith {
            get {
                NullabilityNode result = this;
                while (result.replacement != null)
                    result = result.replacement;
                return result;
            }
        }

        /// <summary>
        /// Replace with node with another node.
        /// All future attempts to create an edge involving this node, will instead create an edge with the other node.
        /// This method can only be used in the NodeBuilding phase, as otherwise there might already be edges registered;
        /// which will not be re-pointed.
        /// </summary>
        internal void ReplaceWith(NullabilityNode other)
        {
            if (this == other) {
                return;
            }
            if (this.replacement != null) {
                this.replacement.ReplaceWith(other);
                return;
            }
            while (other.replacement != null) {
                other = other.replacement;
            }
            Debug.Assert(this.NullType == other.NullType || this.NullType == NullType.Infer || other.NullType == NullType.Infer);
            // Replacements must be performed before the edges are registered.
            Debug.Assert(this.IncomingEdges.Count == 0);
            Debug.Assert(this.OutgoingEdges.Count == 0);
            this.replacement = other;
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

        public override string Name => NullType switch {
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

    /// <summary>
    /// A node that does not correspond to any syntactic construct, but is helpful when constructing the graph.
    /// </summary>
    [DebuggerDisplay("{Name}")]
    internal sealed class HelperNullabilityNode : NullabilityNode
    {
        private static long nextId;
        private readonly long id = Interlocked.Increment(ref nextId);

        public override Location? Location => null;

        private string symbolName = "helper";

        public override string Name => $"<{symbolName}#{id}>";

        internal override void SetName(string name)
        {
            if (symbolName == "helper")
                symbolName = name;
        }
    }
}
