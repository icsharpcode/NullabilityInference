# C# 8 nullability inference

This is a prototype for an algorithm that modifies C# code in order to minimize the number of warnings caused by enabling C# 8.0 nullable reference types.
If this ever gets out of the prototype stage, this might be a useful tool when migrate existing C# code to C# 8.0.

## Usage
  * Update your project to use C# 8.0: `<LangVersion>8.0</LangVersion>`
  * Enable nullable reference types: `<Nullable>enable</Nullable>`
  * If possible, update referenced libraries to newer versions that have nullability annotations.
  * Compile the project and notice that you get a huge bunch of nullability warnings.
  * Run `InferNull myproject.csproj`. This modifies your code by inserting `?` in various places.
  * Compile your project again. You should get a smaller (and hopefully manageable) number of nullability warnings.

For this initial prototype, I'm focusing on self-contained code: any calls to other library (even .NET framework itself) are not properly considered by the analysis.

## Tips+Tricks:

 * The inference tool will only add/remove `?` annotations on nullable reference types. It will never touch your code in any other way.
   * Existing `?` annotations on nullable reference types are discarded and inferred again from scratch.
 * Unconstrained generic types are not reference types, and thus will never be annotated by the tool.
 * The inference tool will not introduce any of the [advanced nullability attributes](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.codeanalysis).
   * However, if these attributes are used in the input code, the tool will in some cases use them for better inference results.
   * It can be useful to annotate generic code with these attributes before running the inference tool.
 * The inference tool acts on one project (`.csproj`) at a time. For best results, any referenced assemblies should already use nullability annotations.
   * If using the tool on multiple projects; apply the tool in the build order.
   * For the .NET base class library, use .NET Core 3 (or later), or use [ReferenceAssemblyAnnotator](https://github.com/tunnelvisionlabs/ReferenceAssemblyAnnotator).
   * For third-party libraries, consider upgrading to a newer version of the library if that adds nullability annotations.

## The algorithm

Let's start with a simple example:

```
class C {
	string key;
	string value;
	
	public C(string key, string value)
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
		C c = new C("abc", null);
		return c.GetHashCode();
	}
}
```

We'll use `!` for types known to be non-null, `?` for types known to be nullable, and `#n` for nullabilities that will need to be inferred by the algorithm.
We construct a global "flow graph" for these nullabilities.
If there's an assignment `a = b`, we create an edge from `b`'s type to `a`'s type.
If there's an assignment `b = null`, we create an edge from a special `nullable` node to `b`'s type.
On a dereference `a.M();`, we create an edge from `a`'s type to a special `nonnull` node (unless the dereference is protected by `if (a != null)`).

```
class C {
	string#1 key;
	string#2 value;
	
	public C(string#3 key, string#4 value)
	{
		this.key = key;     // edge #3 -> #1
		this.value = value; // edge #4 -> #2
	}
	
	public override int GetHashCode()
	{
		return key.GetHashCode(); // edge #1 -> nonnull
	}
	
	public static int Main()
	{
		C#5 c = new C!("abc", null); // edge nonnull -> #5, nonnull -> #3, nullable -> #4
		return c.GetHashCode();      // edge #5 -> nonnull
	}
}
```

Clearly, everything reachable from the `nullable` node should be marked as nullable.
Similarly, everything that can reach the `nonnull` node should be marked as non-nullable.

If the graph contains a path from the `nullable` node to the `nonnull`, we will be unable to create nullability annotations that allow compiling the code without warning.
In such a situation, we would like to minimize the number of warnings instead.
We do this by using the Ford-Fulkerson algorithm to compute the minimum cut (=minimum set of edges to be removed from the graph) so
that the `nonnull` node is no longer reachable from the `nullable` node.
This separates the graph into essentially three parts:
  * nodes reachable from `nullable` --> must be made nullable
  * nodes that reach `nonnull` --> must not be made nullable
  * remaining nodes --> either choice would work

The removed edges correspond to the assignments that will produce warnings after we insert `?` for the types inferred as nullable.
Thus the minimum cut ends up finding a solution that minimizes the number of compiler warnings.

For the remaining nodes where either choice would work, we mark all nodes occurring in "input positions" (e.g. parameters) as nullable.
Then we propagate this nullability along the outgoing edges.
Any nodes that still remain indeterminate after that, are marked as non-nullable.
