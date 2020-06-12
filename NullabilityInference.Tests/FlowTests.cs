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
            // temporary node.
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
    }
}
