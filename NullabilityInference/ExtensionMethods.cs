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
                int arity = nt.ContainingType.FullArity();
                if (type.IsAnonymousType) {
                    arity += nt.GetMembers().OfType<IPropertySymbol>().Count();
                } else {
                    arity += nt.Arity;
                }
                return arity;
            } else if (type is IArrayTypeSymbol || type is IPointerTypeSymbol) {
                return 1;
            } else {
                return 0;
            }
        }

        /// <summary>
        /// Gets the full arity of the method, including the number of type parameters inherited from outer methods.
        /// </summary>
        public static int FullArity(this IMethodSymbol method)
        {
            if (method.ContainingSymbol is IMethodSymbol outerMethod)
                return outerMethod.FullArity() + method.Arity;
            else
                return method.Arity;
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
            if (type.IsAnonymousType) {
                // For anonymous types, we act as if the member types are all type arguments.
                // This lets us track the nullability of indiviual anonymous type members.
                foreach (var member in type.GetMembers()) {
                    if (member is IPropertySymbol prop)
                        yield return prop.Type;
                }
            } else {
                foreach (var typeArg in type.TypeArguments)
                    yield return typeArg;
            }
        }

        /// <summary>
        /// Gets the full list of type arguments for the type symbol, including the number of type arguments inherited from outer methods.
        /// </summary>
        public static IEnumerable<ITypeSymbol> FullTypeArguments(this IMethodSymbol method)
        {
            if (method.ContainingSymbol is IMethodSymbol outerMethod) {
                foreach (var outerTypeArg in outerMethod.FullTypeArguments())
                    yield return outerTypeArg;
            }
            foreach (var typeArg in method.TypeArguments)
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
            if (type.IsAnonymousType) {
                foreach (var member in type.GetMembers()) {
                    if (member is IPropertySymbol)
                        yield return NullableAnnotation.None;
                }
            } else {
                foreach (var annotation in type.TypeArgumentNullableAnnotations)
                    yield return annotation;
            }
        }

        public static IEnumerable<SimpleTypeParameter> FullTypeParameters(this INamedTypeSymbol type)
        {
            if (type.ContainingType != null) {
                foreach (var inheritedTypeParam in type.ContainingType.FullTypeParameters())
                    yield return inheritedTypeParam;
            }
            if (type.IsAnonymousType) {
                foreach (var member in type.GetMembers()) {
                    if (member is IPropertySymbol) {
                        yield return new SimpleTypeParameter();
                    }
                }
            } else {
                foreach (var tp in type.TypeParameters)
                    yield return new SimpleTypeParameter(tp);
            }
        }

        public static IEnumerable<ITypeParameterSymbol> FullTypeParameters(this IMethodSymbol method)
        {
            if (method.ContainingSymbol is IMethodSymbol outerMethod) {
                foreach (var tp in outerMethod.FullTypeParameters())
                    yield return tp;
            }
            foreach (var tp in method.TypeParameters)
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
                if (tp.ContainingSymbol?.ContainingSymbol is IMethodSymbol outerMethod) {
                    return tp.Ordinal + outerMethod.FullArity();
                } else {
                    return tp.Ordinal;
                }
            }
        }

        public static bool CanBeMadeNullable(this ITypeSymbol type)
        {
            if (type is ITypeParameterSymbol tp) {
                // Type parameters can be reference types without having the ": class" constraint,
                // e.g. when there's a "T : BaseClass" constraint.
                // However the "T?" syntax requires an actual "T:class" constraint.
                if (!tp.HasReferenceTypeConstraint)
                    return false;
                if (tp.ReferenceTypeConstraintNullableAnnotation == NullableAnnotation.Annotated) {
                    // we can't use "T?" if T itself might already be a nullable reference type
                    return false;
                }
                // Moreover, this constraint must be syntactic, it is not sufficient if inherited from
                // an overridden method.
                if (tp.TypeParameterKind == TypeParameterKind.Method) {
                    var method = (IMethodSymbol)tp.ContainingSymbol;
                    if (method.IsOverride)
                        return false;
                }
                return true;
            }
            return type.IsReferenceType;
        }

        public static bool IsSystemNullable(this ITypeSymbol? type)
        {
            return type?.OriginalDefinition.SpecialType == SpecialType.System_Nullable_T;
        }

        /// <summary>
        /// Gets the return type used for "return" statements within the method.
        /// </summary>
        public static ITypeSymbol EffectiveReturnType(this IMethodSymbol method)
        {
            // See also: ExtractTaskReturnType()
            var returnType = method.ReturnType;
            if (method.IsAsync && returnType is INamedTypeSymbol namedType && namedType.TypeArguments.Length == 1) {
                returnType = namedType.TypeArguments.Single();
            }
            return returnType;
        }

        /// <summary>
        /// Remove item at index <c>index</c> in O(1) by swapping it with the last element in the collection before removing it.
        /// Useful when the order of elements in the list is not relevant.
        /// </summary>
        public static void SwapRemoveAt<T>(this List<T> list, int index)
        {
            int removeIndex = list.Count - 1;
            list[index] = list[removeIndex];
            list.RemoveAt(removeIndex);
        }
    }
}
