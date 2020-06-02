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
using System.Threading;

namespace ICSharpCode.NullabilityInference
{
    /// <summary>
    /// Implements the Ford-Fulkerson algorithm for maximum flow.
    /// </summary>
    internal static class MaximumFlow
    {
        public static int Compute(IEnumerable<NullabilityNode> allNodes, NullabilityNode source, NullabilityNode sink, CancellationToken cancellationToken)
        {
            Debug.Assert(source != sink);
            int maxFlow = 0;
            ResetVisited(allNodes);
            while (AddFlow(sink, source)) {
                cancellationToken.ThrowIfCancellationRequested();
                maxFlow += 1;
                ResetVisited(allNodes);
            }
            return maxFlow;
        }

        private static bool AddFlow(NullabilityNode node, NullabilityNode source)
        {
            if (node.Visited)
                return false;
            node.Visited = true;
            if (node == source)
                return true;
            var predecessors = node.ResidualGraphPredecessors;
            for (int i = 0; i < predecessors.Count; i++) {
                var prevNode = predecessors[i];
                if (AddFlow(prevNode, source)) {
                    // Remove the edge from the residual graph
                    predecessors.SwapRemoveAt(i);
                    // and instead add the reverse edge
                    prevNode.ResidualGraphPredecessors.Add(node);
                    return true;
                }
            }
            return false;
        }

        private static void ResetVisited(IEnumerable<NullabilityNode> allTypes)
        {
            foreach (NullabilityNode node in allTypes)
                node.Visited = false;
        }
    }
}
