// Copyright (c) 2020 Daniel Grunwald

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Threading;
using System.Threading.Tasks;
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
                Assert.Empty(message + " " + detailMessage);
            }
        }

        protected static (CSharpCompilation, NullCheckingEngine) CompileAndAnalyze(string program, CancellationToken cancellationToken = default)
        {
            var syntaxTree = SyntaxFactory.ParseSyntaxTree(program, new CSharpParseOptions(LanguageVersion.CSharp8), cancellationToken: cancellationToken);
            var options = new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary);
            var compilation = CSharpCompilation.Create("test", new[] { syntaxTree }, defaultReferences.Value, options);
            compilation = AllNullableSyntaxRewriter.MakeAllReferenceTypesNullable(compilation, cancellationToken);
            foreach (var diag in compilation.GetDiagnostics(cancellationToken)) {
                Assert.False(diag.Severity == DiagnosticSeverity.Error, diag.ToString());
            }
            var engine = new NullCheckingEngine(compilation);
            engine.Analyze(cancellationToken);
            return (compilation, engine);
        }
    }
}
