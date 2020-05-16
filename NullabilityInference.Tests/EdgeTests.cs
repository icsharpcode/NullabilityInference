// Copyright (c) 2020 Daniel Grunwald

using Xunit;

namespace ICSharpCode.CodeConverter.Tests.NullabilityInference
{
    /// <summary>
    /// Unit tests where we check that an edge from parameter to return type was detected.
    /// </summary>
    public class EdgeTests : NullabilityTestHelper
    {
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
        public void UseLocalVar()
        {
            Assert.True(HasPathFromParameterToReturnType(@"
class Program {
    public static string Test(string input)
    {
        var local = input;
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
        public void UseAutoProperty()
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
        public void UseAutoPropertyWithNullCheck()
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
        public void UseCustomPropertyGetter()
        {
            Assert.True(HasPathFromParameterToReturnType(@"
class Program {
    static string field;

    static string Property { get { return field; } }

    public static string Test(string input)
    {
        field = input;
        return Property;
    }
}"));
        }

        [Fact]
        public void UseCustomPropertySetter()
        {
            Assert.True(HasPathFromParameterToReturnType(@"
class Program {
    static string field;

    static string Property { set { field = value; } }

    public static string Test(string input)
    {
        Property = input;
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

        [Fact]
        public void ListElement()
        {
            Assert.True(HasPathFromParameterToReturnType(@"
class Program {
    public static string Test(string input) {
        var list = new System.Collections.Generic.List<string>();
        list.Add(input);
        return list[0];
    }
}"));
        }

        [Fact]
        public void ListElementViaCollectionInit()
        {
            Assert.True(HasPathFromParameterToReturnType(@"
class Program {
    public static string Test(string input) {
        var list = new System.Collections.Generic.List<string> { input };
        return list[0];
    }
}"));
        }

        [Fact]
        public void ArrayElement()
        {
            Assert.True(HasPathFromParameterToReturnType(@"
class Program {
    public static string Test(string input) {
        var arr = new string[1];
        arr[0] = input;
        return arr[0];
    }
}"));
        }

        [Fact]
        public void ArrayElementViaArrayInitializer()
        {
            Assert.True(HasPathFromParameterToReturnType(@"
class Program {
    public static string Test(string input) {
        var arr = new string[1] { input };
        return arr[0];
    }
}"));
        }

        [Fact]
        public void CallIndexer()
        {
            Assert.True(HasPathFromParameterToReturnType(@"
class Program {
    public string Test(string input) => this[input];
    public string this[string input] => input;
}"));
        }
    }
}
