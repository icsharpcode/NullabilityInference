using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.CodeAnalysis;

namespace ICSharpCode.NullabilityInference
{
    /// <summary>
    /// Used for ExtensionMethods.FullTypeParameters()
    /// </summary>
    internal struct SimpleTypeParameter
    {
        /// <summary>
        /// If symbol != null, wraps a real type parameter.
        /// If symbol == null, this is a fake type parameter for an anonymous type.
        /// </summary>
        private ITypeParameterSymbol? symbol;

        public SimpleTypeParameter(ITypeParameterSymbol symbol)
        {
            this.symbol = symbol;
        }

        public VarianceKind Variance => symbol?.Variance ?? VarianceKind.None;
        public bool HasNotNullConstraint => symbol?.HasNotNullConstraint ?? false;
    }
}
