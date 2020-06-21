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
using System.Diagnostics;
using Microsoft.CodeAnalysis;

namespace ICSharpCode.NullabilityInference
{
    /// <summary>
    /// An edge in the graph of nullability nodes.
    /// 
    /// An edge represents the constraint "if source is nullable, then target is nullable"
    /// (source is assignable to target).
    /// </summary>
    [DebuggerDisplay("{Source} -> {Target}")]
    internal sealed class NullabilityEdge
    {
        public NullabilityNode Source { get; }
        public NullabilityNode Target { get; }

        internal readonly EdgeLabel Label;
        public bool IsError => Source.NullType == NullType.Nullable && Target.NullType == NullType.NonNull;

        /// <summary>
        /// Represents the subtype relation "subType &lt;: superType".
        /// This means that values of type subType can be assigned to variables of type superType.
        /// </summary>
        public NullabilityEdge(NullabilityNode source, NullabilityNode target, EdgeLabel label)
        {
            this.Source = source ?? throw new ArgumentNullException(nameof(source));
            this.Target = target ?? throw new ArgumentNullException(nameof(target));
            this.Label = label;
        }
    }

    internal readonly struct EdgeLabel
    {
        private readonly string text;
        internal readonly Location? location;

        public EdgeLabel(string text)
        {
            this.text = text;
            this.location = null;
        }

        public EdgeLabel(string text, Location? location)
        {
            this.text = text;
            this.location = location;
        }

        internal EdgeLabel(string text, SyntaxNode? syntaxForLocation)
            : this(text, syntaxForLocation?.GetLocation())
        {
        }

        internal EdgeLabel(string text, IOperation? operation)
            : this(text, operation?.Syntax)
        {
        }

        public override string ToString()
        {
            if (location == null)
                return text;
            else
                return text + " at " + location.StartPosToString();
        }
    }
}
