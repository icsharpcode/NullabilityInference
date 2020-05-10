using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using ICSharpCode.CodeConverter.Util;
using NullabilityInference;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Xunit;

namespace ICSharpCode.CodeConverter.Tests.NullabilityInference
{
    /// <summary>
    /// Unit tests where we check that an edge from parameter to return type was detected.
    /// </summary>
    public class EdgeTests
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

        private static bool HasPathFromParameterToReturnType(string program)
        {
            var syntaxTree = SyntaxFactory.ParseSyntaxTree(program);
            var options = new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary);
            var compilation = CSharpCompilation.Create("test", new[] { syntaxTree }, defaultReferences.Value, options);
            foreach (var diag in compilation.GetDiagnostics()) {
                Assert.False(diag.Severity == DiagnosticSeverity.Error, diag.ToString());
            }
            var engine = new NullCheckingEngine(compilation);
            engine.Analyze(CancellationToken.None);
            var programClass = compilation.GetTypeByMetadataName("Program");
            Assert.False(programClass == null, "Could not find 'Program' in test");
            var testMethod = programClass!.GetMembers("Test").Single();
            var parameterNode = engine.TypeSystem.GetSymbolType(testMethod.GetParameters().Single()).Node;
            var returnNode = engine.TypeSystem.GetSymbolType(testMethod).Node;
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

        [Fact]
        public void SimpleReturn()
        {
            Assert.True(HasPathFromParameterToReturnType(@"
class Program {
    public static string Test(string input)
    {
        return input;
    }
}"));
        }

        [Fact]
        public void SimpleExpressionReturn()
        {
            Assert.True(HasPathFromParameterToReturnType(@"
class Program {
    public static string Test(string input) => input;
}"));
        }

        [Fact]
        public void ReturnConstant()
        {
            Assert.False(HasPathFromParameterToReturnType(@"
class Program {
    public static string Test(string input)
    {
        return ""abc"";
    }
}"));
        }

        [Fact]
        public void UseLocal()
        {
            Assert.True(HasPathFromParameterToReturnType(@"
class Program {
    public static string Test(string input)
    {
        string local = input;
        return local;
    }
}"));
        }

        [Fact]
        public void UseField()
        {
            Assert.True(HasPathFromParameterToReturnType(@"
class Program {
    static string field;

    public static string Test(string input)
    {
        field = input;
        return field;
    }
}"));
        }

        [Fact]
        public void Call()
        {
            Assert.True(HasPathFromParameterToReturnType(@"
class Program {
    public static string Test(string input) => Identity(input);
    public static string Identity(string input) => input;
}"));
        }

    }
}
