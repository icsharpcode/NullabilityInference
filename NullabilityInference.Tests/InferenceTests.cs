﻿// Copyright (c) 2020 Daniel Grunwald
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

namespace ICSharpCode.NullabilityInference.Tests
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
        public void AssertField()
        {
            CheckPaths(@"
using System;
class Program {
    string field;
    public void Init()
    {
        field = string.Empty;
    }
    public string Test(bool b)
    {
        if (b) {
            Assert(field != null);
        } else {
            field = Fallback();
        }
        return field;
    }
    string Fallback() => string.Empty;
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
}", returnNullable: false);
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
        public void MethodGroupCaptureVariantReturn()
        {
            AssertNullabilityInference(@"
using System;
using System.Collections.Generic;

class Program
{
    public Func<IEnumerable<string?>> VarianceInReturnType() => MakeNullList;
    List<string?> MakeNullList() => new List<string?> { null };
}");
        }

        [Fact]
        public void MethodGroupCaptureVariantParameter()
        {
            AssertNullabilityInference(@"
using System;

class Program
{
    public Func<string?, bool> VarianceInParameter() => IsNull;
    bool IsNull(object? o) => o != null;
    public void Call() => VarianceInParameter()(null);
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
        public void ConstructNewNullableValueDoesNotThrow()
        {
            AssertNullabilityInference(@"
using System;

public class SomeContext
{
    public object NullableValue = new int?(3);
}
");
        }

        [Fact]
        public void CompareNullableValueTypeToNullDoesNotThrow()
        {
            string program = @"
public class SomeContext {
    public bool IsNull(int? a) => a != null;
}";
            AssertNullabilityInference(expectedProgram: program, inputProgram: program);
        }

        [Fact]
        public void CompoundAssign()
        {
            string program = @"
public class Program {
    public Program? Test() {
        Program? p = this;
        p += 1;
        return p;
    }
    public static Program? operator +(Program? a, int b) => null;
}";
            AssertNullabilityInference(expectedProgram: program, inputProgram: program);
        }

        [Fact]
        public void LiftedOverloadedOperatorDoesNotThrow()
        {
            string program = @"
public class Program {
    public MyStruct? Test(MyStruct? x, MyStruct? y) => x + y;
}
public struct MyStruct {
    public static MyStruct operator +(MyStruct a, MyStruct b) => default;
}";
            AssertNullabilityInference(expectedProgram: program, inputProgram: program);
        }


        [Fact]
        public void ConditionalAccessOnNullableValueTypeDoesNotThrow()
        {
            string program = @"
public class Program {
    public int? Test(MyStruct? x) => x?.GetLength();
}
public struct MyStruct {
    public int GetLength() => 0;
}";
            AssertNullabilityInference(expectedProgram: program, inputProgram: program);
        }


        [Fact]
        public void ConditionalAccessExtensionMethodOnNullableValueTypeDoesNotThrow()
        {
            string program = @"
public class Program {
    public string? Test(MyStruct? x) => x?.GetText();
}
public struct MyStruct { }
public static class MyExtensions {
    public static string GetText(this MyStruct m) => string.Empty;
}";
            AssertNullabilityInference(expectedProgram: program, inputProgram: program);
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


        [Fact]
        public void ConversionOperator()
        {
            AssertNullabilityInference(@"
using System;
class StringBox {
    string value;

    public StringBox(string? val) { value = val ?? string.Empty; }

    public static implicit operator string?(StringBox? box)
    {
        return box?.value;
    }

    public static string? ImplicitCast()
    {
        return new StringBox(null);
    }

    public static string? ExplicitCast()
    {
        return (string?)new StringBox(null);
    }
}
");
        }

        [Fact]
        public void OverrideMethodInGenericClass()
        {
            AssertNullabilityInference(@"
using System;
abstract class Base<T> {
    public abstract T Get(string input);
}
class Derived : Base<string?> {
    public override string? Get(string input)
    {
        return input.Length == 0 ? null : input;
    }
}");
        }

        [Fact]
        public void DelegateAsSystemDelegate()
        {
            AssertNullabilityInference(@"
using System;
class Program {
    public void A() {
        Invoke((Action)delegate {});
    }

    public void Invoke(Delegate d) {
        d.DynamicInvoke();
    }
}");
        }

        [Fact]
        public void QueryExpression()
        {
            AssertNullabilityInference(@"
using System;
using System.Linq;
class Program {
    public string? Test(string? input) {
        return (
            from line in new[] { input }
            where !(line?.Length > 10)
            select Identity(line)
        ).FirstOrDefault();
    }
    static string? Identity(string? x) => x;
}");
        }

        [Fact]
        public void Lock()
        {
            AssertNullabilityInference(@"class Program {
    public void Test(object obj)
    {
        lock (obj) { }
    }
}");
        }


        [Fact]
        public void MethodCallInGenericClass()
        {
            AssertNullabilityInference(@"
class DataStructure<T> {
    class Node {
        public Node? prev, next;
    }

    static Node? Sibling(Node n) => n.next;

    Node? Test(Node n) => Sibling(n);
}");
        }

        [Fact]
        public void UnusualCasts()
        {
            AssertNullabilityInference(@"
using System.Collections.Generic;
class DataStructure<T> {
    public bool ThisIsString() => (this as DataStructure<char>) != null;
    public bool IsString(IEnumerable<T>? x) => (x as string) != null;
}");
        }

        [Fact]
        public void ObjectInitializer()
        {
            AssertNullabilityInference(@"
struct A {
    public string S1;
    public string? S2;
    public B B;
}
struct B {
    public string S3;
    public string? S4;
}
class Program {
    public static A Create() {
        return new A {
            S1 = string.Empty,
            S2 = null,
            B = { 
                S3 = string.Empty,
                S4 = null
            }
        };
    }
}");
        }

        [Fact]
        public void ListMemberInitializer()
        {
            AssertNullabilityInference(@"
using System.Collections.Generic;
class A {
    public List<string?> Elements = new List<string?>();
}
class Program {
    public static A Create() {
        return new A {
            Elements = {
                string.Empty, 
                null
            }
        };
    }
}");
        }

        [Fact]
        public void GenericOverrideNonNull()
        {
            AssertNullabilityInference(@"
using System;
public abstract class Base<T> where T : class {
    public abstract int Test(T x);
}
class Derived : Base<string> {
    public override int Test(string x) => x.Length;
}
");
        }

        [Fact]
        public void DictTryGetValue()
        {
            AssertNullabilityInference(@"
using System;
using System.Collections.Generic;
class Program {
    public int Test(Dictionary<string, string> d) {
        string? val;
        if (d.TryGetValue(string.Empty, out val)) {
            return val.Length;
        } else {
            return 0;
        }
    }
}
");
        }

        [Fact]
        public void WarningCloseToDeref()
        {
            AssertNullabilityInference(@"
class Program {
    public static int Main() {
        string? n = null;
        string? a = n;
        string? b = a;
        return b.Length;
    }
}");
        }

        [Fact]
        public void InheritedConstraint()
        {
            AssertNullabilityInference(@"
using System;
class Base {
	public virtual void Test<T>(T a) where T : class, IDisposable {
        a.Dispose();
    }
}
class Derived : Base {
	public override void Test<T>(T a) { }
}");
        }

        [Fact]
        public void Pointers()
        {
            AssertNullabilityInference(@"
using System;
struct Box<T> {
    public void Set(T a) {}
}
class Program {
    public unsafe void Test1(Box<string>* a, Box<string?>* b) {
        a->Set(string.Empty);
        b->Set(null);
    }
    public unsafe void Test2(Box<string>* a, Box<string?>* b) {
        (*a).Set(string.Empty);
        (*b).Set(null);
    }
    public unsafe void Test3(Box<string>* a, Box<string?>* b) {
        a[0].Set(string.Empty);
        b[0].Set(null);
    }
}");
        }

        [Fact]
        public void PropertyWithNullCheck()
        {
            AssertNullabilityInference(@"
using System;
class Program {
	string name = string.Empty;
	public string Name {
		get { return this.name; }
		set {
			if (value == null)
				throw new ArgumentNullException(nameof(value));
			this.name = value;
		}
	}
}");
        }

        [Fact]
        public void TypeOfUnbound()
        {
            AssertNullabilityInference(@"
using System;
class Program {
	public Type SomeType() => typeof(Func<>);
}");
        }

        [Fact]
        public void IsExpression1()
        {
            AssertNullabilityInference(@"
using System.Collections.Generic;
class Program {
    public static bool Test(object? input) {
        return input is Program;
    }
}");
        }

        [Fact]
        public void IsPattern1()
        {
            AssertNullabilityInference(@"
using System.Collections.Generic;
class Program {
    public static bool Test(object? input) {
        return input is {};
    }
}");
        }

        [Fact]
        public void IsPattern2()
        {
            AssertNullabilityInference(@"
using System.Collections.Generic;
class Program {
    public static bool Test(object? input) {
        return input is Program p && p.ToString() == ""Program"";
    }
 }");
        }

        [Fact]
        public void IsPattern3()
        {
            AssertNullabilityInference(@"
using System;
class Program {
	public string Test(object? x) => x is string s ? s : string.Empty;
}");
        }

        [Fact]
        public void IsPattern4()
        {
            AssertNullabilityInference(@"
using System;
using System.Collections.Generic;
class Program {
	public void Test(object? x) {
        if (x is List<string?> s1) s1.Add(null);
        if (x is List<string> s2) s2.Add(string.Empty);
    }
}");
        }

        [Fact]
        public void IsPattern5()
        {
            AssertNullabilityInference(@"
using System;
using System.Collections.Generic;
class Program {
	public void Test(object? x) {
        if (x is List<string?> { Count: 0 } s1) s1.Add(null);
        if (x is List<string> { Count: 2 } s2) s2.Add(string.Empty);
    }
}");
        }

        [Fact]
        public void IsPattern6()
        {
            AssertNullabilityInference(@"
using System;
using System.Collections.Generic;
class Box<T> {
    public T Value { get; set; } 
    public Box(T val) { Value = val; }
}
class Program {
	public int Test(object? x) {
        if (x is Box<string> { Value: var b })
            return b.Length;
        return 0;
    }
}");
        }

        [Fact]
        public void StringInterpolation()
        {
            AssertNullabilityInference(@"
using System;
class Program {
	public string Test(object? x, string y) {
        return $""{x}: {y.Length}"";
    }
}");
        }


        [Fact]
        public void ExplicitInterfaceImplsOnlyDifferingInTypeArgs()
        {
            string program = @"
using System.Collections;
using System.Collections.Generic;
public class MyList : IEnumerable<string?>, IEnumerable<(string, string?)> {
    IEnumerator<string?> IEnumerable<string?>.GetEnumerator() {
        yield return null;
    }

    IEnumerator<(string, string?)> IEnumerable<(string, string?)>.GetEnumerator() {
        yield return (string.Empty, null);
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        throw new System.NotImplementedException();
    }
}";
            AssertNullabilityInference(expectedProgram: program, inputProgram: program);
        }

        [Fact]
        public void WhereImpl()
        {
            AssertNullabilityInference(@"
using System;
using System.Collections.Generic;
class Program {
	public static IEnumerable<T> Where<T>(IEnumerable<T> input, Predicate<T> filter) where T : class {
		foreach (var member in input) {
			if (filter(member))
				yield return member;
		}
    }

    static void Test() {
        string[] arr = { string.Empty };
        foreach (string a in Where(arr, s => s.Length > 0)) {
        }
    }
}");
        }

        [Fact]
        public void NullablePragma()
        {
            string program = @"
class Program {
#nullable enable
    public void A(string x) {}
    public void B(string? x) {}
#nullable disable
    public void C(string x) {}
#nullable restore
    public void D(string x) => A(x);
    public void E(string? x) => B(x);
    public void F(string? x) => C(x);
}";
            AssertNullabilityInference(program, program);
        }

        [Fact]
        public void NullablePragmaWithCode()
        {
            string program = @"
class Program {
#nullable enable
    public void Test()
    {
        string a = string.Empty; 
        string? b = null;
        A(ref a);
        B(ref b);
    }
#nullable restore
    public void A(ref string x) {}
    public void B(ref string? x) {}
}";
            AssertNullabilityInference(program, program);
        }

        [Fact]
        public void DeconstructNonTuple()
        {
            AssertNullabilityInference(@"
public class AnyContext
{
    public void Caller()
    {
        (float f, int i) = new Deconstructable<float>(3f);
    }
}

public class Deconstructable<T>
{
    public Deconstructable(T t) => _t = t;
    private T _t;

    public void Deconstruct(out T x, out int y)
    {
        x = _t;
        y = 5;
    }
}");
        }

        [Fact]
        public void SeeAlsoCref()
        {
            AssertNullabilityInference(@"
class Program {
    public int A(string x, string? y) { return x.Length; }

    /// <seealso cref=""Program""/>
    /// <seealso cref=""A(string, string?)""/>
    /// <seealso cref=""Program.A(string, string?)""/>
    public void B() { }
}");
        }

        [Fact]
        public void AsyncMethods()
        {
            AssertNullabilityInference(@"
using System.Threading.Tasks;
class Program {
    async Task<string?> GetNull() => null;
    async Task<string> GetEmpty() => string.Empty;
}");
        }

        [Fact]
        public void LocalMethodInGenericMethod()
        {
            AssertNullabilityInference(@"
class Program {
    public T Test<T>()
    {
        return Get();

        T Get() => default;
    }
}");
        }
    }
}
