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
using System.Collections.Immutable;
using System.ComponentModel.DataAnnotations;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using ICSharpCode.NullabilityInference;
using InferNull.FromRoslynSdk;
using McMaster.Extensions.CommandLineUtils;
using Microsoft.Build.Locator;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Host;
using Microsoft.CodeAnalysis.Host.Mef;
using Microsoft.CodeAnalysis.MSBuild;
using Microsoft.VisualStudio.Composition;

namespace InferNull
{
    [Command(Name = "infernull", Description = "Tool for inferring C# 8 nullable reference types for existing C# code bases",
          ExtendedHelpText = @"
Remarks:
  Adds nullability annotations to a single project (.csproj).
  Please backup / commit your files to source control before use.
  We recommend running the conversion in-place (i.e. not specifying an output directory) for best performance.
  See https://github.com/icsharpcode/NullabilityInference for the source code, issues and other info.
")]
    [HelpOption("-h|--help")]
    internal class Program
    {
        public static Task<int> Main(string[] args) => CommandLineApplication.ExecuteAsync<Program>(args);

        [FileExists]
        [Required]
        [Argument(0, "Project file name", "The project (.csproj file) for which to infer nullability. This argument is mandatory.")]
        public string ProjectName { get; } = string.Empty;

        [Option("-n|--dry-run", "Do not write result back to disk.", CommandOptionType.NoValue)]
        public bool DryRun { get; }

        [Option("-f|--force", "Allow overwriting uncommitted changes", CommandOptionType.NoValue)]
        public bool Force { get; }

        [Option("--all-nullable", "Don't run inference, just mark all reference types as nullable.", CommandOptionType.NoValue)]
        public bool AllNullable { get; }

        [Option("-e|--add-nullable-enable", "Add '#nullable enable' to the top of each source file.", CommandOptionType.NoValue)]
        public bool AddNullableEnable { get; }

        [Option("-s|--strategy", "Select how conflicts are resolved when a node both could be assigned null, but is also used where a non-nullable value is required. (-s:MinimizeWarnings, -s:PreferNull, -s:PreferNotNull). "
             + "The default is MinimizeWarnings.", CommandOptionType.SingleValue)]
        public ConflictResolutionStrategy Strategy { get; } = ConflictResolutionStrategy.MinimizeWarnings;

#if DEBUG
        [Option("-g|--show-graph", "Show type graph. Requires GraphViz dot.exe in PATH.", CommandOptionType.NoValue)]
        public bool ShowGraph { get; }

        [Option("--filter-graph", "Apply filter, showing only a portion of the graph.", CommandOptionType.MultipleValue)]
        public List<string> FilterGraph { get; } = new List<string>();

        [Option("--export-graph", "Save type graph to file.", CommandOptionType.SingleValue)]
        public string? ExportGraph { get; } = null;
#endif

        /// <remarks>Used by reflection in CommandLineApplication.ExecuteAsync</remarks>
        private async Task<int> OnExecuteAsync(CommandLineApplication _)
        {
            var outputDirectory = new DirectoryInfo(Path.GetDirectoryName(ProjectName));
            if (await CouldOverwriteUncommittedFilesAsync(outputDirectory)) {
                await Console.Error.WriteLineAsync($"WARNING: There are files in {outputDirectory.FullName} which may be overwritten, and aren't committed to git");
                if (Force) {
                    await Console.Error.WriteLineAsync("Continuing with possibility of data loss due to force option.");
                } else {
                    await Console.Error.WriteLineAsync("Aborting to avoid data loss (see above warning). Commit the files to git, or use the --force option to override this check.");
                    return 1;
                }
            }

            var buildProps = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase) {
                ["Configuration"] = "Debug",
                ["Platform"] = "AnyCPU"
            };

            var cancellationToken = CancellationToken.None;
            using var workspace = await CreateWorkspaceAsync(buildProps);
            await Console.Error.WriteLineAsync("Loading project...");
            Project project;
            try {
                project = await workspace.OpenProjectAsync(ProjectName, cancellationToken: cancellationToken);
            } catch (Exception ex) {
                await Console.Error.WriteLineAsync(ex.ToString());
                return 1;
            }
            await Console.Error.WriteLineAsync("Compiling...");
            var compilation = await project.GetCompilationAsync(cancellationToken) as CSharpCompilation;
            if (compilation == null) {
                await Console.Error.WriteLineAsync("project.GetCompilationAsync() did not return CSharpCompilation");
                return 1;
            }
            compilation = AllNullableSyntaxRewriter.MakeAllReferenceTypesNullable(compilation, cancellationToken);
            if (AllNullable) {
                await Console.Error.WriteLineAsync("Writing modified code...");
                foreach (var tree in compilation.SyntaxTrees) {
                    WriteTree(tree, cancellationToken);
                }
                return 0;
            }
            bool hasErrors = false;
            foreach (var diag in compilation.GetDiagnostics(cancellationToken)) {
                if (diag.Severity == DiagnosticSeverity.Error) {
                    await Console.Error.WriteLineAsync(diag.ToString());
                    hasErrors = true;
                }
            }
            if (hasErrors) {
                await Console.Error.WriteLineAsync("Compilation failed. Cannot infer nullability.");
                return 1;
            }
            await Console.Error.WriteLineAsync("Inferring nullabilities...");
            var engine = new NullCheckingEngine(compilation);
            engine.Analyze(this.Strategy, cancellationToken);
#if DEBUG
            GraphVizGraph? exportedGraph = null;
            if (ShowGraph) {
                await Console.Error.WriteLineAsync("Showing graph...");
                exportedGraph ??= ExportTypeGraph(engine);
                    exportedGraph.Show();
            }
            if (ExportGraph != null) {
                await Console.Error.WriteLineAsync("Exporting graph...");
                exportedGraph ??= ExportTypeGraph(engine);
                exportedGraph.Save(ExportGraph);
            }
#endif
            Statistics stats;
            if (DryRun) {
                await Console.Error.WriteLineAsync("Computing statistics...");
                stats = engine.ConvertSyntaxTrees(cancellationToken, tree => { });
                await Console.Error.WriteLineAsync("Analysis successful. Results are discarded due to --dry-run.");
                await Console.Error.WriteLineAsync("Would use:");
            } else {
                await Console.Error.WriteLineAsync("Writing modified code...");
                stats = engine.ConvertSyntaxTrees(cancellationToken, tree => WriteTree(tree, cancellationToken));
                await Console.Error.WriteLineAsync("Success!");
                await Console.Error.WriteLineAsync("Used:");
            }
            await Console.Error.WriteLineAsync($"  {stats.NullableCount} nullable reference types.");
            await Console.Error.WriteLineAsync($"  {stats.NonNullCount} non-nullable reference types.");
            await Console.Error.WriteLineAsync($"  {stats.NotNullWhenCount} [NotNullWhen] attributes.");

            return 0;
        }

#if DEBUG
        private GraphVizGraph ExportTypeGraph(NullCheckingEngine engine)
        {
            if (FilterGraph.Count == 0) {
                // Show complete graph
                return engine.ExportTypeGraph();
            } else {
                // Show filtered graph
                var list = new List<(string file, int start, int end)>();
                foreach (var entry in FilterGraph) {
                    var m = Regex.Match(entry, @"^([\w_.-]+):(\d+)-(\d+)$");
                    if (!m.Success) {
                        Console.WriteLine("Invalid value for --show-graph/--export-graph. Expected filename.cs:100-200");
                    }
                    list.Add((m.Groups[1].Value, int.Parse(m.Groups[2].Value), int.Parse(m.Groups[3].Value)));
                }
                return engine.ExportTypeGraph(location => list.Any(e => MatchesEntry(e, location)));

                static bool MatchesEntry((string file, int start, int end) entry, Location loc)
                {
                    var span = loc.GetLineSpan();
                    if (span.EndLinePosition.Line + 1 < entry.start)
                        return false;
                    if (entry.end < span.StartLinePosition.Line + 1)
                        return false;
                    return string.Equals(Path.GetFileName(span.Path), entry.file, StringComparison.OrdinalIgnoreCase);
                }
            }
        }
#endif

        private void WriteTree(SyntaxTree tree, CancellationToken cancellationToken)
        {
            if (string.IsNullOrEmpty(tree.FilePath))
                return;
            if (AddNullableEnable) {
                var root = tree.GetRoot(cancellationToken);
                if (!root.GetLeadingTrivia().Any(trivia => trivia.IsKind(SyntaxKind.NullableDirectiveTrivia))) {
                    var newDirective = SyntaxFactory.Trivia(
                        SyntaxFactory.NullableDirectiveTrivia(
                            settingToken: SyntaxFactory.Token(SyntaxKind.EnableKeyword).WithLeadingTrivia(SyntaxFactory.Whitespace(" ")),
                            isActive: true
                        ).WithTrailingTrivia(SyntaxFactory.EndOfLine(Environment.NewLine))
                    );
                    root = root.WithLeadingTrivia(new[] { newDirective }.Concat(root.GetLeadingTrivia()));
                    tree = tree.WithRootAndOptions(root, tree.Options);
                }
            }
            using var stream = new FileStream(tree.FilePath, FileMode.Create, FileAccess.Write);
            using var writer = new StreamWriter(stream, tree.Encoding);
            writer.Write(tree.GetText(cancellationToken));
        }

        private static async Task<MSBuildWorkspace> CreateWorkspaceAsync(Dictionary<string, string> buildProps)
        {
            if (MSBuildLocator.CanRegister) {
                var instances = MSBuildLocator.QueryVisualStudioInstances().ToArray();
                var instance = instances.OrderByDescending(x => x.Version).FirstOrDefault()
                    ?? throw new ValidationException("No Visual Studio instance available");
                MSBuildLocator.RegisterInstance(instance);
                AppDomain.CurrentDomain.UseVersionAgnosticAssemblyResolution();
            }

            var hostServices = await CreateHostServicesAsync(MSBuildMefHostServices.DefaultAssemblies);
            return MSBuildWorkspace.Create(buildProps, hostServices);
        }

        private static async Task<bool> CouldOverwriteUncommittedFilesAsync(DirectoryInfo outputDirectory)
        {
            if (!await IsInsideGitWorkTreeAsync(outputDirectory))
                return true; // unversioned files might be overwritten
            return !await IsGitDiffEmptyAsync(outputDirectory);
        }

        private static async Task<bool> IsInsideGitWorkTreeAsync(DirectoryInfo outputDirectory)
        {
            var gitDiff = new ProcessStartInfo("git") {
                Arguments = ArgumentEscaper.EscapeAndConcatenate(new[] { "rev-parse", "--is-inside-work-tree" }),
                WorkingDirectory = outputDirectory.FullName
            };

            using var stdout = new StringWriter();
            int exitCode = await gitDiff.GetExitCodeAsync(stdout, stdErr: TextWriter.Null);
            return exitCode == 0;
        }

        private static async Task<bool> IsGitDiffEmptyAsync(DirectoryInfo outputDirectory)
        {
            var gitDiff = new ProcessStartInfo("git") {
                Arguments = ArgumentEscaper.EscapeAndConcatenate(new[] { "diff", "--exit-code", "--relative", "--summary", "--diff-filter=ACMRTUXB*" }),
                WorkingDirectory = outputDirectory.FullName
            };

            using var stdout = new StringWriter();
            int exitCode = await gitDiff.GetExitCodeAsync(stdout);
            if (exitCode == 1) Console.WriteLine(stdout.ToString());
            return exitCode == 0;
        }


        /// <summary>
        /// Use this in all workspace creation
        /// </summary>
        private static async Task<HostServices> CreateHostServicesAsync(ImmutableArray<Assembly> assemblies)
        {
            var exportProvider = await CreateExportProviderFactoryAsync(assemblies);
            return MefHostServices.Create(exportProvider.CreateExportProvider().AsCompositionContext());
        }

        private static async Task<IExportProviderFactory> CreateExportProviderFactoryAsync(ImmutableArray<Assembly> assemblies)
        {
            var discovery = new AttributedPartDiscovery(Resolver.DefaultInstance, isNonPublicSupported: true);
            var parts = await discovery.CreatePartsAsync(assemblies);
            var catalog = ComposableCatalog.Create(Resolver.DefaultInstance).AddParts(parts);

            var configuration = CompositionConfiguration.Create(catalog);
            var runtimeComposition = RuntimeComposition.CreateRuntimeComposition(configuration);
            return runtimeComposition.CreateExportProviderFactory();
        }
    }
}
