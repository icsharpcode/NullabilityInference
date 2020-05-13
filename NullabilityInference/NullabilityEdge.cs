// Copyright (c) 2020 Daniel Grunwald

using System;
using System.Diagnostics;
using Microsoft.CodeAnalysis;

namespace NullabilityInference
{
    /// <summary>
    /// An edge in the graph of nullability nodes.
    /// </summary>
    [DebuggerDisplay("{Source} -> {Target}")]
    internal sealed class NullabilityEdge
    {
        public NullabilityNode Source { get; private set; }
        public NullabilityNode Target { get; private set; }

#if DEBUG
        internal string? Label;
#endif
        internal int Capacity = 1;
        internal int ReverseCapacity;
        public bool IsError => Source.NullType == NullType.Nullable && Target.NullType == NullType.NonNull;

        /// <summary>
        /// Represents the subtype relation "subType &lt;: superType".
        /// This means that values of type subType can be assigned to variables of type superType.
        /// </summary>
        public NullabilityEdge(NullabilityNode source, NullabilityNode target)
        {
            this.Source = source ?? throw new ArgumentNullException(nameof(source));
            this.Target = target ?? throw new ArgumentNullException(nameof(target));
        }

        [Conditional("DEBUG")]
        internal void SetLabel(string text, Location? location)
        {
#if DEBUG
            if (location == null)
                this.Label = text;
            else
                this.Label = text + " at " + location.StartPosToString();
#endif
        }
    }
}
