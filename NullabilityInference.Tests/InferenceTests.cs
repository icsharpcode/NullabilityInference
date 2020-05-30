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
        public void AliasQualifiedName()
        {
            AssertNullabilityInference(@"
class Generic<T> { public static T Value { get; set; } }
class Program {
    public static void Main() {
        global::Generic<string?>.Value = null;
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
        public void StaticEventInit()
        {
            AssertNullabilityInference(@"
using System;
class Program {
    static event EventHandler? uninit;
    static event EventHandler init_nonnull = delegate {};
    static event EventHandler? init_null = null;
    static event EventHandler init_nonnull_cctor;
    static event EventHandler? init_null_cctor;

    static Program() {
        init_nonnull_cctor = delegate{};
        init_null_cctor = null;
    }

    Program() {
        uninit = delegate{}; // too late, field will still be marked nullable
    }
}");
        }

        [Fact]
        public void InstanceEventInit()
        {
            AssertNullabilityInference(@"
using System;
class Program {
    event EventHandler? uninit;
    event EventHandler init_nonnull = delegate {};
    event EventHandler? init_null = null;
    event EventHandler init_nonnull_ctor;
    event EventHandler? init_null_ctor;

    Program() {
        init_nonnull_ctor = delegate {};
        init_null_ctor = null;
    }

    void Set() {
        uninit = delegate {}; // too late, field will still be marked nullable
    }
}");
        }


        [Fact]
        public void NoConstructor()
        {
            AssertNullabilityInference(@"
using System;
class Program {
    static string? uninit_field { get; set; }
    static string init_field { get; set; } = """";
    static string? uninit_prop { get; set; }
    static string init_prop { get; set; } = """";
    event EventHandler? uninit_event;
    event EventHandler init_event = delegate{};

    string? uninit_instance_field { get; set; }
    string init_instance_field { get; set; } = """";
    string? uninit_instance_prop { get; set; }
    string init_instance_prop { get; set; } = """";
    event EventHandler? uninit_instance_event;
    event EventHandler init_instance_event = delegate{};
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
        public void CallInheritedMethod()
        {
            AssertNullabilityInference(@"
using System.Collections.Generic;
class GenericList : List<(string, string?)> {
    public void AddDummyEntry() => Add((string.Empty, null));
}");
        }

        [Fact]
        public void EnumWithExplicitInherit()
        {
            AssertNullabilityInference(@"
enum MyEnum : int {
    FirstMember
}");
        }

        [Fact]
        public void AsCast()
        {
            AssertNullabilityInference(@"
class Program {
    public string? Test(object x)
    {
        x.ToString();
        return x as string;
    }
}");
        }

        [Fact]
        public void Cast()
        {
            AssertNullabilityInference(@"
class Program {
    public string Test(object x)
    {
        x.ToString();
        return (string)x;
    }
}");
        }

        [Fact]
        public void ImplementInterfaceProperty()
        {
            AssertNullabilityInference(@"
interface I {
    string A { get; }
    string? B { get; }
}
class Program : I {
    public string A => string.Empty;
    public string? B => null;
}");
        }

        [Fact]
        public void RegisterEventHandler()
        {
            AssertNullabilityInference(@"
using System;
class Timer {
    public event EventHandler? Tick;
}
class Program {
    public void Test(Timer t)
    {
        t.Tick += delegate(object? sender, EventArgs e) {};
    }
}");
        }

        [Fact]
        public void CreateNullLambda()
        {
            AssertNullabilityInference(@"
using System;
class Program {
    public Func<string?> ImplicitCast() => () => null;
    public Func<string?> ExplicitCast() => (Func<string?>)(() => null);
    public Func<string?> NewDelegate() => new Func<string?>(() => null);
}");
        }

        [Fact]
        public void MethodGroup()
        {
            AssertNullabilityInference(@"
using System;
class Program {
    public Func<string?> ImplicitCast(object o) => o.ToString;
    public Func<string?> ExplicitCast(object o) => (Func<string?>)o.ToString;
    public Func<string?> NewDelegate(object o) => new Func<string?>(o.ToString);
}");
        }

        [Fact]
        public void GenericMethodGroup()
        {
            AssertNullabilityInference(@"
using System;
class Program {
    public EventHandler MethodGroup() => GenericMethod<object?, EventArgs>;
    void GenericMethod<A, B>(A a, B b) {}
}");
        }

        [Fact]
        public void VarianceInMethodGroupCapture()
        {
            AssertNullabilityInference(@"
using System;
using System.Collections.Generic;

class Program
{
    public Func<IEnumerable<string?>> VarianceInMethodGroupCapture() => MakeNullList;
    List<string?> MakeNullList() => new List<string?> { null };
}");
        }

        [Fact]
        public void TypeAlias()
        {
            AssertNullabilityInference(@"
using Alias = System.Collections.Generic.List<string?>;

class Program
{
    Alias Test1() => new Alias { null };
    Alias? Test2() => null;
}
");
        }

        [Fact]
        public void ConstructNewNullableValue()
        {
            AssertNullabilityInference(@"
using System;

public class SomeContext
{
    public Nullable<int> NullableValue = 1.ToString() != 2.ToString() ? default(int?) : new int?(3);
}
");
        }

        [Fact]
        public void EventsInInterface()
        {
            AssertNullabilityInference(@"
using System;
public interface I
{
    event EventHandler<EventArgs> A;
    event EventHandler<EventArgs?> B;
}
public class Program : I
{
    public event EventHandler<EventArgs>? A;
    public event EventHandler<EventArgs?>? B;

    public void Invoke()
    {
        A?.Invoke(this, EventArgs.Empty);
        B?.Invoke(this, null);
    }
}");
        }

        [Fact]
        public void ExplicitEventInterfaceImpl()
        {
            AssertNullabilityInference(@"
using System;
public interface I
{
    event EventHandler<EventArgs> A;
    event EventHandler<EventArgs?> B;
}
public class Program : I
{
    EventHandler<EventArgs>? a;
    EventHandler<EventArgs?>? b;

    event EventHandler<EventArgs> I.A { add { a += value; } remove { a -= value; } }
    event EventHandler<EventArgs?> I.B { add { b += value; } remove { b -= value; } }

    public void Invoke()
    {
        a?.Invoke(this, EventArgs.Empty);
        b?.Invoke(this, null);
    }
}");
        }
    }
}
