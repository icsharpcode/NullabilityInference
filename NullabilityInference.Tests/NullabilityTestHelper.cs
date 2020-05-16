// Copyright (c) 2020 Daniel Grunwald

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using ICSharpCode.CodeConverter.Util;
using NullabilityInference;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Xunit;

namespace ICSharpCode.CodeConverter.Tests.NullabilityInference
{
    public class NullabilityTestHelper
    {
        private static readonly string refAsmPath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86),
            @"Reference Assemblies\Microsoft\Framework\.NETFramework\v4.7.2");
        private static readonly Lazy<IEnumerable<MetadataReference>> defaultReferences = new Lazy<IEnumerable<MetadataReference>>(delegate {
            return new[]
            {
                    MetadataReference.CreateFromFile(Path.Combine(refAsmPath, "Facades\\netstandard.dll")),
                    MetadataReference.CreateFromFile(Path.Combine(refAsmPath, "mscorlib.dll")),
                    MetadataReference.CreateFromFile(Path.Combine(refAsmPath, "System.dll")),
                    MetadataReference.CreateFromFile(Path.Combine(refAsmPath, "System.Core.dll")),
                    MetadataReference.CreateFromFile(Path.Combine(refAsmPath, @"Facades\System.Runtime.dll")),
                    MetadataReference.CreateFromFile(Path.Combine(refAsmPath, "System.Xml.dll")),
                    MetadataReference.CreateFromFile(Path.Combine(refAsmPath, "Microsoft.CSharp.dll")),
                    MetadataReference.CreateFromFile(typeof(ValueTuple).Assembly.Location),
                    MetadataReference.CreateFromFile(typeof(ValueTask).Assembly.Location),
                    MetadataReference.CreateFromFile(typeof(Span<>).Assembly.Location),
            };
        });

        static NullabilityTestHelper()
        {
            Debug.Listeners.Insert(0, new TestTraceListener());
        }

        private class TestTraceListener : DefaultTraceListener
        {
            public override void Fail(string message, string detailMessage)
            {
                throw new InvalidOperationException(message + " " + detailMessage);
            }
        }

        protected static (CSharpCompilation, NullCheckingEngine) CompileAndAnalyze(string program, CancellationToken cancellationToken = default)
        {
            var syntaxTree = SyntaxFactory.ParseSyntaxTree(program, new CSharpParseOptions(LanguageVersion.CSharp8), cancellationToken: cancellationToken);
            var options = new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary);
            var compilation = CSharpCompilation.Create("test", new[] { syntaxTree }, defaultReferences.Value, options);
            compilation = AllNullableSyntaxRewriter.MakeAllReferenceTypesNullable(compilation, cancellationToken);
            string allNullableText = compilation.SyntaxTrees.Single().GetText().ToString();
            foreach (var diag in compilation.GetDiagnostics(cancellationToken)) {
                Assert.False(diag.Severity == DiagnosticSeverity.Error, diag.ToString());
            }
            var engine = new NullCheckingEngine(compilation);
            engine.Analyze(cancellationToken);
            return (compilation, engine);
        }

        protected static void AssertNullabilityInference(string expectedProgram, string inputProgram = null, CancellationToken cancellationToken = default)
        {
            inputProgram ??= expectedProgram.Replace("?", "");
            var (_, engine) = CompileAndAnalyze(inputProgram, cancellationToken);
            var newSyntax = engine.ConvertSyntaxTrees(cancellationToken).Single();
            string outputProgram = newSyntax.GetText(cancellationToken).ToString();
            // engine.ExportTypeGraph().Show();
            Assert.Equal(expectedProgram, outputProgram);
        }

        protected static bool HasPathFromParameterToReturnType(string program)
        {
            var (compilation, engine) = CompileAndAnalyze(program);
            var programClass = compilation.GetTypeByMetadataName("Program");
            Assert.False(programClass == null, "Could not find 'Program' in test");
            var testMethod = programClass!.GetMembers("Test").Single();
            var parameterNode = engine.TypeSystem.GetSymbolType(testMethod.GetParameters().Single()).Node;
            var returnNode = engine.TypeSystem.GetSymbolType(testMethod).Node;
            // engine.ExportTypeGraph().Show();
            return ReachableNodes(parameterNode, n => n.Successors).Contains(returnNode);
        }

        private static HashSet<T> ReachableNodes<T>(T root, Func<T, IEnumerable<T>> successors)
        {
            var visited = new HashSet<T>();
            Visit(root);
            return visited;

            void Visit(T node)
            {
                if (visited.Add(node)) {
                    foreach (var successor in successors(node)) {
                        Visit(successor);
                    }
                }
            }
        }
    }
}
