// Copyright (c) 2020 Daniel Grunwald

using System;
using System.Collections.Generic;
using System.Linq;
using ICSharpCode.CodeConverter.Util;
using Xunit;

namespace ICSharpCode.CodeConverter.Tests.NullabilityInference
{
    /// <summary>
    /// Unit tests where we check that an edge from parameter to return type was detected.
    /// </summary>
    public class EdgeTests :  NullabilityTestHelper
    {
        public static bool HasPathFromParameterToReturnType(string program)
        {
            var (compilation, engine) = CompileAndAnalyze(program);
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
        public void NullCheck()
        {
            Assert.False(HasPathFromParameterToReturnType(@"
class Program {
    public static string Test(string input)
    {
        if (input == null) {
            return ""null"";
        }
        return input;
    }
}"));
        }

        [Fact]
        public void UseLocalWithNullCheck()
        {
            Assert.False(HasPathFromParameterToReturnType(@"
class Program {
    public static string Test(string input)
    {
        string local = input;
        if (local == null) {
            return ""null"";
        }
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
        public void UseFieldWithNullCheck()
        {
            Assert.False(HasPathFromParameterToReturnType(@"
class Program {
    static string field;

    public static string Test(string input)
    {
        field = input;
        if (field == null) {
            return ""null"";
        }
        return field;
    }
}"));
        }

        [Fact]
        public void UseProperty()
        {
            Assert.True(HasPathFromParameterToReturnType(@"
class Program {
    static string Property { get; set; }

    public static string Test(string input)
    {
        Property = input;
        return Property;
    }
}"));
        }

        [Fact]
        public void UsePropertyWithNullCheck()
        {
            Assert.False(HasPathFromParameterToReturnType(@"
class Program {
    static string Property { get; set; }

    public static string Test(string input)
    {
        Property = input;
        if (Property == null) {
            return ""null"";
        }
        return Property;
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

        [Fact]
        public void CallExtensionMethod()
        {
            Assert.True(HasPathFromParameterToReturnType(@"
static class Program {
    public static string Test(string input) => input.Identity();
    public static string Identity(this string input) => input;
}"));
        }

        [Fact]
        public void NullCoalescingLeft()
        {
            Assert.False(HasPathFromParameterToReturnType(@"
class Program {
    public static string Test(string input) => input ?? ""abc"";
}"));
        }


        [Fact]
        public void NullCoalescingRight()
        {
            Assert.True(HasPathFromParameterToReturnType(@"
class Program {
    static string field;
    public static string Test(string input) => field ?? input;
}"));
        }

        [Fact]
        public void NullCoalescingWithThrowExpr()
        {
            Assert.False(HasPathFromParameterToReturnType(@"
class Program {
    public static string Test(string input) => input ?? throw null;
}"));
        }
    }
}
