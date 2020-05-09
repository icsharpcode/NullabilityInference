using System;
using System.Collections.Generic;
using System.Text;

namespace NullabilityInference
{
    public enum NullType
    {
        /// <summary>
        /// Let the static null checker automatically infer whether this type is nullable.
        /// </summary>
        Infer = 0,
        /// <summary>
        /// Declares the type as non-nullable.
        /// </summary>
        NonNull = 1,
        /// <summary>
        /// Declares the type as nullable.
        /// </summary>
        Nullable = 2,
        /// <summary>
        /// Declares the type as oblivious.
        /// </summary>
        Oblivious = 3
    }
}
