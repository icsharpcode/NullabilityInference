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
        public static int Compute(IEnumerable<NullabilityNode> allTypes, NullabilityNode source, NullabilityNode sink, CancellationToken cancellationToken)
        {
            Debug.Assert(source != sink);
            int maxFlow = 0;
            int newFlow;
            ResetVisited(allTypes);
            while ((newFlow = AddFlow(source, sink, int.MaxValue)) > 0) {
                cancellationToken.ThrowIfCancellationRequested();
                maxFlow += newFlow;
                ResetVisited(allTypes);
            }
            return maxFlow;
        }

        private static int AddFlow(NullabilityNode node, NullabilityNode sink, int maxNewFlow)
        {
            if (maxNewFlow == 0 || node.Visited)
                return 0;
            node.Visited = true;
            if (node == sink)
                return maxNewFlow;
            foreach (NullabilityEdge edge in node.OutgoingEdges) {
                int newFlow = AddFlow(edge.Target, sink, Math.Min(maxNewFlow, edge.Capacity));
                if (newFlow > 0) {
                    edge.Capacity -= newFlow;
                    edge.ReverseCapacity += newFlow;
                    return newFlow;
                }
            }
            foreach (NullabilityEdge edge in node.IncomingEdges) {
                int newFlow = AddFlow(edge.Source, sink, Math.Min(maxNewFlow, edge.ReverseCapacity));
                if (newFlow > 0) {
                    edge.ReverseCapacity -= newFlow;
                    edge.Capacity += newFlow;
                    return newFlow;
                }
            }
            return 0;
        }

        private static void ResetVisited(IEnumerable<NullabilityNode> allTypes)
        {
            foreach (NullabilityNode node in allTypes)
                node.Visited = false;
        }
    }
}
