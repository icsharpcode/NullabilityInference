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
// DEALINGS IN THE SOFTWARE

using System.Runtime.Remoting.Proxies;
using Xunit;

namespace ICSharpCode.NullabilityInference.Tests
{
    public class FlowTests : NullabilityTestHelper
    {
        [Fact]
        public void ReturnNotNullViaNullableVar()
        {
            CheckPaths(@"
class Program {
    public static string Test() {
        string? a = null;
        a = string.Empty;
        return a;
    }
}", returnNullable: false);
        }

        [Fact]
        public void ReturnInputViaNullableVar()
        {
            CheckPaths(@"
class Program {
    public static string Test(string input) {
        string? a = null;
        a = input;
        return a;
    }
}", returnNullable: false, returnDependsOnInput: true);
        }

        [Fact]
        public void IfFlow1()
        {
            CheckPaths(@"
class Program {
    static bool b;
    public static string Test(string input) {
        string? a = null;
        if (b) {
            a = input;
        }
        return a;
    }
}", returnNullable: true);
            // Note: the path from parameter to return doesn't appear in the graph,
            // because the flow-state on the else-branch is <nullable>,
            // and Join(<nullable>, x) directly returns <nullable> without introducing a
            // helper node.
        }

        [Fact]
        public void IfFlow2()
        {
            CheckPaths(@"
class Program {
    static bool b;
    public static string Test(string input) {
        string? a = null;
        if (b) {
            a = input;
        } else {
            a = string.Empty;
        }
        return a;
    }
}", returnNullable: false, returnDependsOnInput: true);
        }

        [Fact]
        public void IfFlow3()
        {
            CheckPaths(@"
using System;
class Program {
    static bool b;
    public static string Test(string input) {
        string? a = null;
        if (b) {
            a = input;
        } else {
            throw new Exception();
        }
        return a;
    }
}", returnNullable: false, returnDependsOnInput: true);
        }


        [Fact]
        public void GetOrCreateNode()
        {
            // 'node' local variable must be nullable, but GetNode return type can be non-nullable.
            AssertNullabilityInference(@"
using System.Collections.Generic;
class DataStructure
{
    class Node { }

    Dictionary<string, Node> mapping = new Dictionary<string, Node>();

    Node GetNode(string element)
    {
        Node? node;
        if (!mapping.TryGetValue(element, out node))
        {
            node = new Node();
            mapping.Add(element, node);
        }
        return node;
    }

}");
        }

        [Fact]
        public void GetOrCreateNodeWithOutVarDecl()
        {
            // 'node' local variable must be nullable, but GetNode return type can be non-nullable.
            AssertNullabilityInference(@"
using System.Collections.Generic;
class DataStructure
{
    class Node { }

    Dictionary<string, Node> mapping = new Dictionary<string, Node>();

    Node GetNode(string element)
    {
        if (!mapping.TryGetValue(element, out Node? node))
        {
            node = new Node();
            mapping.Add(element, node);
        }
        return node;
    }

}");
        }

        [Fact]
        public void ConditionalAnd()
        {
            AssertNullabilityInference(@"
using System.Collections.Generic;
class DataStructure
{
    class Node { }

    Dictionary<string, Node> mapping = new Dictionary<string, Node>();

    Node GetNode(string element)
    {
        Node? node;
        if (mapping.TryGetValue(element, out node) && IsValid(node))
        {
            return node;
        }
        return new Node();
    }

    bool IsValid(Node? n) => true;
}");
        }

        [Fact]
        public void CoalesceAssign()
        {
            CheckPaths(@"
class Program {
    public static string Test(string input) {
        string? a = null;
        a ??= input;
        return a;
    }
}", returnNullable: false, returnDependsOnInput: true);
        }

        [Fact]
        public void InferNotNullWhenTrue()
        {
            string program = @"
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
class DataStructure
{
    Dictionary<string, string> dict = new Dictionary<string, string>();

    public bool TryGetValue(string key, [Attr] out string? val)
    {
        return dict.TryGetValue(key, out val);
    }
}";
            AssertNullabilityInference(
                expectedProgram: program.Replace("[Attr]", "[NotNullWhen(true)]"),
                inputProgram: program.Replace("[Attr] ", ""));
        }

        [Fact]
        public void InferNotNullWhenFalse()
        {
            string program = @"
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
class DataStructure
{
    Dictionary<string, string> dict = new Dictionary<string, string>();

    public bool TryGetValue(string key, [Attr] out string? val)
    {
        return !dict.TryGetValue(key, out val);
    }
}";
            AssertNullabilityInference(
                expectedProgram: program.Replace("[Attr]", "[NotNullWhen(false)]"),
                inputProgram: program.Replace("[Attr] ", ""));
        }

        [Fact]
        public void InferNotNullWhenTrueFromControlFlow()
        {
            string program = @"
using System.Collections.Generic;
[using]
class DataStructure
{
    public bool TryGet(int i, [Attr] out string? name)
    {
        if (i > 0)
        {
            name = string.Empty;
            return true;
        }
        name = null;
        return false;
    }
}";
            AssertNullabilityInference(
                expectedProgram: program.Replace("[Attr]", "[NotNullWhen(true)]").Replace("[using]", "using System.Diagnostics.CodeAnalysis;"),
                inputProgram: program.Replace("[Attr] ", "").Replace("[using]", ""));
        }

        [Fact]
        public void InferNotNullWhenTrueFromComparisonInReturn()
        {
            string program = @"
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
class DataStructure
{
    public bool TryGet(string? input, [Attr] out string? name)
    {
        name = input;
        return name != null;
    }
}";
            AssertNullabilityInference(
                expectedProgram: program.Replace("[Attr]", "[NotNullWhen(true)]"),
                inputProgram: program.Replace("[Attr] ", ""));
        }

        [Fact]
        public void UseNotNullIfNotNull()
        {
            string program = @"
using System.Diagnostics.CodeAnalysis;
class Program
{
	public void Test()
    {
		string a = Identitity(string.Empty);
		string? b = Identitity(null);
    }

#nullable enable
	[return: NotNullIfNotNull(""input"")]
    public static string? Identitity(string? input) => input;
}";
            AssertNullabilityInference(program, program);
        }
    }
}
