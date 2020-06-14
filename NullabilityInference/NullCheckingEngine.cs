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
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;

namespace ICSharpCode.NullabilityInference
{
    /// <summary>
    /// Determines how to handle conflicted nodes (nodes that could be assigned null, but that also are used where a non-null type is required).
    /// </summary>
    public enum ConflictResolutionStrategy
    {
        /// <summary>
        /// Minimize the number of constraint violations: uses minimum cut of constraint graph.
        /// </summary>
        MinimizeWarnings,
        /// <summary>
        /// Conflicted nodes are marked null.
        /// </summary>
        PreferNull,
        /// <summary>
        /// Conflicted nodes are marked not-null.
        /// </summary>
        PreferNotNull,
    }

    public sealed class NullCheckingEngine
    {
        private readonly CSharpCompilation compilation;
        private readonly TypeSystem typeSystem;

        public TypeSystem TypeSystem => typeSystem;

        /// <summary>
        /// Creates a new NullCheckingEngine instance for the specified compilation.
        /// Note: for Roslyn's flow analysis to be useful to the inference, the given compilation should have as many reference types
        /// annotated as nullable as possible. This can be accomplished by using <see cref="AllNullableSyntaxRewriter.MakeAllReferenceTypesNullable"/>.
        /// </summary>
        public NullCheckingEngine(CSharpCompilation compilation)
        {
            this.compilation = compilation;
            this.typeSystem = new TypeSystem(compilation);
        }

        /// <summary>
        /// Constructs the null-type flow graph and infers nullabilities for the nodes.
        /// </summary>
        public void Analyze(ConflictResolutionStrategy strategy, CancellationToken cancellationToken)
        {
            Parallel.ForEach(compilation.SyntaxTrees,
                new ParallelOptions { CancellationToken = cancellationToken },
                t => CreateNodes(t, cancellationToken));

            Parallel.ForEach(compilation.SyntaxTrees,
                new ParallelOptions { CancellationToken = cancellationToken },
                t => CreateEdges(t, cancellationToken));

            switch (strategy) {
                case ConflictResolutionStrategy.MinimizeWarnings:
                    MaximumFlow.Compute(typeSystem.AllNodes, typeSystem.NullableNode, typeSystem.NonNullNode, cancellationToken);

                    // Infer non-null first using the residual graph.
                    InferNonNullUsingResidualGraph(typeSystem.NonNullNode);
                    // Then use the original graph to infer nullable types everywhere we didn't already infer non-null.
                    // This ends up creating the minimum cut.
                    InferNullable(typeSystem.NullableNode);
                    // Note that for longer chains (null -> A -> B -> C -> nonnull)
                    // this approach ends up cutting the graph as close to nonnull as possible when there's multiple
                    // choices with the same number of warnings. This is why we use the "reverse" residual graph
                    // (ResidualGraphPredecessors) -- using ResidualGraphSuccessors would end up cutting closer to the <null> node.
                    break;
                case ConflictResolutionStrategy.PreferNull:
                    InferNullable(typeSystem.NullableNode);
                    InferNonNull(typeSystem.NonNullNode);
                    break;
                case ConflictResolutionStrategy.PreferNotNull:
                    InferNonNull(typeSystem.NonNullNode);
                    InferNullable(typeSystem.NullableNode);
                    break;
                default:
                    throw new NotSupportedException(strategy.ToString());
            }

            // There's going to be a bunch of remaining nodes where either choice would work.
            // For parameters, prefer marking those as nullable:
            foreach (var paramNode in typeSystem.NodesInInputPositions) {
                if (paramNode.ReplacedWith.NullType == NullType.Infer) {
                    InferNullable(paramNode.ReplacedWith);
                }
            }
            foreach (var node in typeSystem.AllNodes) {
                // Finally, anything left over is inferred to be non-null:
                if (node.NullType == NullType.Infer) {
                    if (node.ReplacedWith.NullType != NullType.Infer)
                        node.NullType = node.ReplacedWith.NullType;
                    else
                        node.NullType = NullType.NonNull;
                }
                Debug.Assert(node.NullType == node.ReplacedWith.NullType);
            }
        }

        private void InferNonNullUsingResidualGraph(NullabilityNode node)
        {
            Debug.Assert(node.NullType == NullType.Infer || node.NullType == NullType.NonNull);
            node.NullType = NullType.NonNull;
            foreach (var pred in node.ResidualGraphPredecessors) {
                if (pred.NullType == NullType.Infer) {
                    InferNonNullUsingResidualGraph(pred);
                }
            }
        }

        /// <summary>
        /// Invokes the callback with the new syntax trees where the inferred nullability has been inserted.
        /// </summary>
        public Statistics ConvertSyntaxTrees(CancellationToken cancellationToken, Action<SyntaxTree> action)
        {
            object statsLock = new object();
            Statistics statistics = new Statistics();
            Parallel.ForEach(compilation.SyntaxTrees,
                new ParallelOptions { CancellationToken = cancellationToken },
                syntaxTree => {
                    var semanticModel = compilation.GetSemanticModel(syntaxTree);
                    var rewriter = new InferredNullabilitySyntaxRewriter(semanticModel, typeSystem, typeSystem.GetMapping(syntaxTree), cancellationToken);
                    var newRoot = rewriter.Visit(syntaxTree.GetRoot());
                    action(syntaxTree.WithRootAndOptions(newRoot, syntaxTree.Options));
                    lock (statsLock) {
                        statistics.Update(rewriter.Statistics);
                    }
                });
            return statistics;
        }

        private void InferNonNull(NullabilityNode node)
        {
            Debug.Assert(node.NullType == NullType.Infer || node.NullType == NullType.NonNull);
            node.NullType = NullType.NonNull;
            foreach (var edge in node.IncomingEdges) {
                if (edge.Source.NullType == NullType.Infer) {
                    InferNonNull(edge.Source);
                }
            }
        }

        private void InferNullable(NullabilityNode node)
        {
            Debug.Assert(node.NullType == NullType.Infer || node.NullType == NullType.Nullable);
            node.NullType = NullType.Nullable;
            foreach (var edge in node.OutgoingEdges) {
                if (edge.Target.NullType == NullType.Infer) {
                    InferNullable(edge.Target);
                }
            }
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
            var tsBuilder = new TypeSystem.Builder(typeSystem);
            var mapping = typeSystem.GetMapping(syntaxTree);
            var visitor = new EdgeBuildingSyntaxVisitor(semanticModel, typeSystem, tsBuilder, mapping, cancellationToken);
            visitor.Visit(syntaxTree.GetRoot(cancellationToken));
            foreach (var cref in mapping.CrefSyntaxes) {
                visitor.HandleCref(cref);
            }
            lock (typeSystem) {
                tsBuilder.Flush(typeSystem);
            }
        }

#if DEBUG
        /// <summary>
        /// Exports the type graph in a form suitable for visualization.
        /// </summary>
        public GraphVizGraph ExportTypeGraph()
        {
            return ExportTypeGraph(n => n.NullType != NullType.Oblivious && n.ReplacedWith == n);
        }

        /// <summary>
        /// Exports a subset of the type graph in a form suitable for visualization.
        /// </summary>
        public GraphVizGraph ExportTypeGraph(Predicate<NullabilityNode> nodeFilter)
        {
            if (nodeFilter == null)
                throw new ArgumentNullException("includeInGraph");
            GraphVizGraph graph = new GraphVizGraph { rankdir = "BT" };
            List<NullabilityEdge> graphEdges = new List<NullabilityEdge>();
            foreach (NullabilityNode node in typeSystem.AllNodes) {
                foreach (NullabilityEdge edge in node.IncomingEdges) {
                    if (nodeFilter(edge.Source) || nodeFilter(edge.Target)) {
                        graphEdges.Add(edge);
                    }
                }
            }
            // Select nodes based on include filter
            IEnumerable<NullabilityNode> includedNodes = typeSystem.AllNodes.Where(n => nodeFilter(n));
            // Add nodes necessary for selected edges
            includedNodes = includedNodes.Concat(graphEdges.Select(g => g.Source)).Concat(graphEdges.Select(g => g.Target)).Distinct();
            var nodeIds = new Dictionary<NullabilityNode, string>();
            foreach (NullabilityNode node in includedNodes) {
                string nodeId = $"n{nodeIds.Count}";
                nodeIds.Add(node, nodeId);
                GraphVizNode gvNode = new GraphVizNode(nodeId) { label = node.Name, fontsize = 32 };
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
                gvNode.label += $"\n{node.NullType}";
                graph.AddNode(gvNode);
            }
            foreach (NullabilityEdge edge in graphEdges) {
                var gvEdge = new GraphVizEdge(nodeIds[edge.Source], nodeIds[edge.Target]);
                gvEdge.label = edge.Label.ToString();
                gvEdge.fontsize = 8;
                if (edge.IsError)
                    gvEdge.color = "red";
                graph.AddEdge(gvEdge);
            }
            return graph;
        }
#endif
    }
}
