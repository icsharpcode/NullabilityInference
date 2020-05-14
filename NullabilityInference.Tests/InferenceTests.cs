// Copyright (c) 2020 Daniel Grunwald

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xunit;

namespace ICSharpCode.CodeConverter.Tests.NullabilityInference
{
    public class InferenceTests : NullabilityTestHelper
    {
        [Fact]
        public void SimpleClass()
        {
            AssertNullabilityInference(@"
internal class C
{
    private readonly string key;
    private readonly string? value;

    public C(string key, string? value)
    {
        this.key = key;
        this.value = value;
    }

    public override int GetHashCode()
    {
        return key.GetHashCode();
    }

    public static int Main()
    {
        C c = new C(""abc"", null);
        return c.GetHashCode();
    }
}");
        }

        [Fact]
        public void NewList()
        {
            AssertNullabilityInference(@"
using System.Collections.Generic;
class Program {
    public static int Main() {
        List<string?> list1 = new List<string?>();
        List<string> list2 = new List<string>();
        list1.Add(null);
        return list2[0].Length;
    }
}");
        }
        
        /*
        [Fact]
        public void StaticMethodCallOnGenericType()
        {
            AssertNullabilityInference(@"
using System.Collections.Generic;
class Generic<T> { public static List<T> MakeList() { return new List<T>(); } }
class Program {
    public static int Main() {
        var list1 = Generic<string?>.MakeList();
        var list2 = Generic<string>.MakeList();
        list1.Add(null);
        return list2[0].Length;
    }
}");
        }
        */
    }
}
