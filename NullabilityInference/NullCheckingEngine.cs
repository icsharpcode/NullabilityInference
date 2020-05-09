using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;

namespace NullabilityInference
{
    public sealed class NullCheckingEngine
    {
        private readonly Compilation compilation;
        private readonly TypeSystem typeSystem;

        public NullCheckingEngine(Compilation compilation)
        {
            this.compilation = compilation;
            this.typeSystem = new TypeSystem(compilation);
        }

        public void Analyze(CancellationToken cancellationToken)
        {
            Parallel.ForEach(compilation.SyntaxTrees,
                new ParallelOptions { CancellationToken = cancellationToken },
                t => CreateNodes(t, cancellationToken));

            Parallel.ForEach(compilation.SyntaxTrees,
                new ParallelOptions { CancellationToken = cancellationToken },
                t => CreateEdges(t, cancellationToken));
        }

        private void CreateNodes(SyntaxTree syntaxTree, CancellationToken cancellationToken)
        {
            var semanticModel = compilation.GetSemanticModel(syntaxTree);
            var tsBuilder = new TypeSystem.Builder(typeSystem);
            var visitor = new NodeBuildingSyntaxVisitor(semanticModel, tsBuilder, cancellationToken);
            visitor.Visit(syntaxTree.GetRoot(cancellationToken));
            lock (typeSystem) {
                typeSystem.RegisterNodes(syntaxTree, visitor.Mapping);
                tsBuilder.Flush(typeSystem);
            }
        }

        private void CreateEdges(SyntaxTree syntaxTree, CancellationToken cancellationToken)
        {
            var semanticModel = compilation.GetSemanticModel(syntaxTree);
            var visitor = new EdgeBuildingSyntaxVisitor(semanticModel, typeSystem, typeSystem.GetMapping(syntaxTree), cancellationToken);
            visitor.Visit(syntaxTree.GetRoot(cancellationToken));
        }

        public GraphVizGraph ExportTypeGraph(Predicate<NullabilityNode> nodeFilter, Predicate<NullabilityEdge> edgeFilter)
        {
            if (nodeFilter == null)
                throw new ArgumentNullException("includeInGraph");
            GraphVizGraph graph = new GraphVizGraph { rankdir = "BT" };
            List<NullabilityEdge> graphEdges = new List<NullabilityEdge>();
            foreach (NullabilityNode node in typeSystem.AllNodes) {
                foreach (NullabilityEdge edge in node.IncomingEdges) {
                    if (edgeFilter(edge) || nodeFilter(edge.Source) || nodeFilter(edge.Target)) {
                        graphEdges.Add(edge);
                    }
                }
            }
            // Select nodes based on include filter
            IEnumerable<NullabilityNode> includedNodes = typeSystem.AllNodes.Where(n => nodeFilter(n));
            // Add nodes necessary for selected edges
            includedNodes = includedNodes.Concat(graphEdges.Select(g => g.Source)).Concat(graphEdges.Select(g => g.Target)).Distinct();
            foreach (NullabilityNode node in includedNodes) {
                GraphVizNode gvNode = new GraphVizNode(Escape(node.Name)) { label = node.Name, fontsize = 32 };
                if (node is SpecialNullabilityNode) {
                    gvNode.fontsize = 24;
                } else {
                    gvNode.fontsize = 12;
                    gvNode.margin = "0.05,0.05";
                    gvNode.height = 0;
                    gvNode.shape = "box";
                }
                if (node.Location != null) {
                    gvNode.label += $"\n{node.Location.EndPosToString()}";
                }
                graph.AddNode(gvNode);
            }
            foreach (NullabilityEdge edge in graphEdges) {
                var gvEdge = new GraphVizEdge(Escape(edge.Source.Name), Escape(edge.Target.Name));
                gvEdge.label = edge.Label;
                gvEdge.fontsize = 8;
                if (edge.IsError)
                    gvEdge.color = "red";
                else if (edge.Capacity == 0)
                    gvEdge.color = "yellow";
                graph.AddEdge(gvEdge);
            }
            return graph;

            string Escape(string name)
            {
                return name.Replace('#', '_');
            }
        }

    }

    internal static class RoslynExtensions
    {
        public static string StartPosToString(this Location location)
        {
            var lineSpan = location.GetLineSpan();
            string filename = System.IO.Path.GetFileName(lineSpan.Path);
            return $"{filename}:{lineSpan.StartLinePosition.Line + 1}:{lineSpan.StartLinePosition.Character + 1}";
        }

        public static string EndPosToString(this Location location)
        {
            var lineSpan = location.GetLineSpan();
            string filename = System.IO.Path.GetFileName(lineSpan.Path);
            return $"{filename}:{lineSpan.EndLinePosition.Line + 1}:{lineSpan.EndLinePosition.Character + 1}";
        }
    }
}
