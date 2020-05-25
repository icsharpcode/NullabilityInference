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

using Xunit;

namespace ICSharpCode.NullabilityInference.Tests.NullabilityInference
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

        [Fact]
        public void Call()
        {
            AssertNullabilityInference(@"
class Program {
    public static void Main() {
        string? a = Identity(null);
        string? b = Identity(""abc"");
    }
    public static string? Identity(string? input) => input;
}");
        }

        [Fact]
        public void GenericCall()
        {
            AssertNullabilityInference(@"
class Program {
    public static void Main() {
        string? n = null;
        string? a = Identity(n);
        string b = Identity(""abc"");
    }
    public static T Identity<T>(T input) => input;
}");
        }

        [Fact]
        public void GenericCallExplicitTypeArguments()
        {
            AssertNullabilityInference(@"
class Program {
    public static void Main() {
        string? n = null;
        string? a = Identity<string?>(n);
        string b = Identity<string>(""abc"");
    }
    public static T Identity<T>(T input) => input;
}");
        }

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

        [Fact]
        public void StaticFieldAccessOnGenericType()
        {
            AssertNullabilityInference(@"
class Generic<T> { public static T Value; }
class Program {
    public static void Main() {
        Generic<string?>.Value = null;
    }
}");
        }

        [Fact]
        public void StaticPropertyAccessOnGenericType()
        {
            AssertNullabilityInference(@"
class Generic<T> { public static T Value { get; set; } }
class Program {
    public static void Main() {
        Generic<string?>.Value = null;
    }
}");
        }

        [Fact]
        public void Array()
        {
            AssertNullabilityInference(@"
class Program {
    public static string?[] Test() {
        var arr = new string?[1];
        arr[0] = null;
        return arr;
    }
}");
        }

        [Fact]
        public void ArrayCast()
        {
            AssertNullabilityInference(@"
class Program {
    public static string?[] Test(object input) {
        var arr = (string?[])input;
        arr[0] = null;
        return arr;
    }
}");
        }

        [Fact]
        public void StaticFieldInit()
        {
            AssertNullabilityInference(@"
class Program {
    static string? uninit;
    static string init_nonnull = ""a"";
    static string? init_null = null;
    static string init_nonnull_cctor;
    static string? init_null_cctor;

    static Program() {
        init_nonnull_cctor = ""a"";
        init_null_cctor = null;
    }

    Program() {
        uninit = ""b""; // too late, field will still be marked nullable
    }
}");
        }

        [Fact]
        public void InstanceFieldInit()
        {
            AssertNullabilityInference(@"
class Program {
    string? uninit;
    string init_nonnull = ""a"";
    string? init_null = null;
    string init_nonnull_ctor;
    string? init_null_ctor;

    Program() {
        init_nonnull_ctor = ""a"";
        init_null_ctor = null;
    }

    void Set() {
        uninit = ""b""; // too late, field will still be marked nullable
    }
}");
        }

        [Fact]
        public void StaticPropertyInit()
        {
            AssertNullabilityInference(@"
class Program {
    static string? uninit { get; set; }
    static string init_nonnull { get; set; } = ""a"";
    static string? init_null { get; set; } = null;
    static string init_nonnull_cctor { get; set; }
    static string? init_null_cctor { get; set; }

    static Program() {
        init_nonnull_cctor = ""a"";
        init_null_cctor = null;
    }

    Program() {
        uninit = ""b""; // too late, field will still be marked nullable
    }
}");
        }

        [Fact]
        public void InstancePropertyInit()
        {
            AssertNullabilityInference(@"
class Program {
    string? uninit { get; set; }
    string init_nonnull { get; set; } = ""a"";
    string? init_null { get; set; } = null;
    string init_nonnull_ctor { get; set; }
    string? init_null_ctor { get; set; }

    Program() {
        init_nonnull_ctor = ""a"";
        init_null_ctor = null;
    }

    void Set() {
        uninit = ""b""; // too late, field will still be marked nullable
    }
}");
        }


        [Fact]
        public void NoConstructor()
        {
            AssertNullabilityInference(@"
class Program {
    static string? uninit_field { get; set; }
    static string init_field { get; set; } = """";
    static string? uninit_prop { get; set; }
    static string init_prop { get; set; } = """";

    string? uninit_instance_field { get; set; }
    string init_instance_field { get; set; } = """";
    string? uninit_instance_prop { get; set; }
    string init_instance_prop { get; set; } = """";
}");
        }

        [Fact]
        public void CallbackOnDispose()
        {
            AssertNullabilityInference(@"
using System;
using System.Threading;
public sealed class CallbackOnDispose : IDisposable
{
	Action? action;

	public CallbackOnDispose(Action action)
	{
		this.action = action ?? throw new ArgumentNullException(nameof(action));
	}

	public void Dispose()
	{
		Interlocked.Exchange<Action?>(ref action, null)?.Invoke();
	}
}");
        }

        [Fact]
        public void UnusedParameters()
        {
            // None of the parameters are dereferenced or stored anywhere.
            // We want a completely unused parameter (a) to default to nullable,
            // but throwing an ArgumentNullException or using a Debug.Assert()
            // should mark the parameter as non-nullable.
            AssertNullabilityInference(@"
using System;
class Program {
    public static void Test(string? a, string b, string c)
    {
        if (b == null)
            throw new ArgumentNullException(""b"");
        Assert(c != null);
    }
    static void Assert([System.Diagnostics.CodeAnalysis.DoesNotReturnIf(parameterValue: false)] bool b)
    {
        while(!b);
    }
}
namespace System.Diagnostics.CodeAnalysis {
    sealed class DoesNotReturnIfAttribute : Attribute {
        public DoesNotReturnIfAttribute(bool parameterValue) => ParameterValue = parameterValue;
        public bool ParameterValue { get; }
    }
}");
        }


        [Fact]
        public void Inheritance()
        {
            AssertNullabilityInference(@"
using System.Collections.Generic;
class GenericList : List<(string, string?)> {
    public void AddDummyEntry() => Add((string.Empty, null));
}");
        }
    }
}
