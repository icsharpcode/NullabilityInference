// Copyright (c) 2020 Daniel Grunwald

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace NullabilityInference
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

        public static string GetFullName(this ISymbol symbol)
        {
            if (symbol.ContainingType != null)
                return symbol.ContainingType.GetFullName() + "." + symbol.Name;
            else if (symbol.ContainingNamespace is { IsGlobalNamespace: false })
                return symbol.ContainingNamespace.GetFullName() + "." + symbol.Name;
            else
                return symbol.Name;
        }
    }
}
