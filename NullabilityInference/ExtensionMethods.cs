using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis;

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
    }
}
