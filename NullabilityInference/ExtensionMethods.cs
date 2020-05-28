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
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace ICSharpCode.NullabilityInference
{
    internal static class ExtensionMethods
    {
        public static string StartPosToString(this Location location)
        {
            var lineSpan = location.GetLineSpan();
            string filename = System.IO.Path.GetFileName(lineSpan.Path);
            return $"{filename}:{lineSpan.StartLinePosition.Line + 1}:{lineSpan.StartLinePosition.Character + 1}";
        }

        public static string EndPosToString(this Location location)
        {
            var lineSpan = location.GetLineSpan();
            string filename = System.IO.Path.GetFileName(lineSpan.Path);
            return $"{filename}:{lineSpan.EndLinePosition.Line + 1}:{lineSpan.EndLinePosition.Character + 1}";
        }

        public static void Deconstruct<K, V>(this KeyValuePair<K, V> pair, out K key, out V value)
        {
            key = pair.Key;
            value = pair.Value;
        }

        public static IEnumerable<(A, B)> Zip<A, B>(this IEnumerable<A> input1, IEnumerable<B> input2)
        {
            return input1.Zip(input2, (a, b) => (a, b));
        }

        public static bool IsAutoProperty(this PropertyDeclarationSyntax syntax)
        {
            if (syntax.ExpressionBody != null || syntax.AccessorList == null || !syntax.AccessorList.Accessors.Any())
                return false;
            foreach (var accessor in syntax.AccessorList.Accessors) {
                if (accessor.Body != null || accessor.ExpressionBody != null)
                    return false;
            }
            return true;
        }

        public static VarianceKind Combine(this (VarianceKind, VarianceKind) variancePair)
        {
            return variancePair switch
            {
                (VarianceKind.None, _) => VarianceKind.None,
                (_, VarianceKind.None) => VarianceKind.None,
                (VarianceKind.Out, VarianceKind.Out) => VarianceKind.Out,
                (VarianceKind.In, VarianceKind.Out) => VarianceKind.In,
                (VarianceKind.Out, VarianceKind.In) => VarianceKind.In,
                (VarianceKind.In, VarianceKind.In) => VarianceKind.Out,
                _ => throw new NotSupportedException("Unknown VarianceKind")
            };
        }

        public static VarianceKind ToVariance(this RefKind refKind)
        {
            return refKind switch
            {
                RefKind.None => VarianceKind.In,
                RefKind.In => VarianceKind.In,
                RefKind.Ref => VarianceKind.None,
                RefKind.Out => VarianceKind.Out,
                _ => throw new NotSupportedException($"RefKind unsupported: {refKind}")
            };
        }

        public static string GetFullName(this ISymbol symbol)
        {
            if (symbol.ContainingType != null)
                return symbol.ContainingType.GetFullName() + "." + symbol.Name;
            else if (symbol.ContainingNamespace is { IsGlobalNamespace: false })
                return symbol.ContainingNamespace.GetFullName() + "." + symbol.Name;
            else
                return symbol.Name;
        }


        /// <summary>
        /// Gets the full arity of the type symbol, including the number of type parameters inherited from outer classes.
        /// </summary>
        public static int FullArity(this ITypeSymbol? type)
        {
            if (type is INamedTypeSymbol nt) {
                return nt.Arity + nt.ContainingType.FullArity();
            } else if (type is IArrayTypeSymbol || type is IPointerTypeSymbol) {
                return 1;
            } else {
                return 0;
            }
        }

        /// <summary>
        /// Gets the full list of type arguments for the type symbol, including the number of type arguments inherited from outer classes.
        /// </summary>
        public static IEnumerable<ITypeSymbol> FullTypeArguments(this INamedTypeSymbol type)
        {
            if (type.ContainingType != null) {
                foreach (var inheritedTypeArg in type.ContainingType.FullTypeArguments())
                    yield return inheritedTypeArg;
            }
            foreach (var typeArg in type.TypeArguments)
                yield return typeArg;
        }

        /// <summary>
        /// Gets the full list of type arguments for the type symbol, including the number of type arguments inherited from outer classes.
        /// </summary>
        public static IEnumerable<NullableAnnotation> FullTypeArgumentNullableAnnotations(this INamedTypeSymbol type)
        {
            if (type.ContainingType != null) {
                foreach (var inheritedTypeArg in type.ContainingType.FullTypeArgumentNullableAnnotations())
                    yield return inheritedTypeArg;
            }
            foreach (var annotation in type.TypeArgumentNullableAnnotations)
                yield return annotation;
        }

        public static IEnumerable<ITypeParameterSymbol> FullTypeParameters(this INamedTypeSymbol type)
        {
            if (type.ContainingType != null) {
                foreach (var inheritedTypeParam in type.ContainingType.FullTypeParameters())
                    yield return inheritedTypeParam;
            }
            foreach (var tp in type.TypeParameters)
                yield return tp;
        }

        /// <summary>
        /// Gets the index of the type parameter in its parent type's FullTypeParameters()
        /// </summary>
        public static int FullOrdinal(this ITypeParameterSymbol tp)
        {
            if (tp.TypeParameterKind == TypeParameterKind.Type) {
                return tp.Ordinal + (tp.ContainingType?.ContainingType.FullArity() ?? 0);
            } else {
                return tp.Ordinal;
            }
        }
    }
}
