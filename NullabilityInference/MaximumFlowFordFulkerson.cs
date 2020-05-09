using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace NullabilityInference
{
    internal static class MaximumFlowFordFulkerson
    {
        public static int Compute(IEnumerable<NullabilityNode> allTypes, NullabilityNode source, NullabilityNode sink)
        {
            Debug.Assert(source != sink);
            int maxFlow = 0;
            int newFlow;
            ResetVisited(allTypes);
            while ((newFlow = AddFlow(source, sink, int.MaxValue)) > 0) {
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
