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
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Operations;

namespace ICSharpCode.NullabilityInference
{
    internal enum AccessPathRoot
    {
        /// <summary>
        /// The first element in the symbol array is a local variable or parameter.
        /// All following symbols are fields/properties.
        /// </summary>
        Local,
        /// <summary>
        /// The access path is rooted in 'this'.
        /// All symbols in the array are fields/properties.
        /// </summary>
        This,
    }

    /// <summary>
    /// Represents a path for which we can track the flow-state.
    /// </summary>
    internal readonly struct AccessPath : IEquatable<AccessPath>
    {
        public readonly AccessPathRoot Root;
        public readonly ImmutableArray<ISymbol> Symbols;


        public AccessPath(AccessPathRoot root, ImmutableArray<ISymbol> symbols)
        {
            this.Root = root;
            this.Symbols = symbols;
        }

        public bool IsParameter => Root == AccessPathRoot.Local && Symbols.Length == 1 && Symbols[0].Kind == SymbolKind.Parameter;

        public static AccessPath? FromOperation(IOperation? operation)
        {
            var list = ImmutableArray.CreateBuilder<ISymbol>();
            list.Reverse();
            while (operation is IMemberReferenceOperation mro && (mro.Kind == OperationKind.PropertyReference || mro.Kind == OperationKind.FieldReference)) {
                if (mro.Member is IPropertySymbol prop && !prop.Parameters.IsEmpty) {
                    return null; // indexers are not supported
                }
                list.Add(mro.Member);
                operation = mro.Instance;
            }
            AccessPathRoot root;
            switch (operation) {
                case ILocalReferenceOperation localOp:
                    list.Add(localOp.Local);
                    root = AccessPathRoot.Local;
                    break;
                case IParameterReferenceOperation paramOp:
                    list.Add(paramOp.Parameter);
                    root = AccessPathRoot.Local;
                    break;
                case IInstanceReferenceOperation { ReferenceKind: InstanceReferenceKind.ContainingTypeInstance }:
                    root = AccessPathRoot.This;
                    break;
                default:
                    return null;
            }
            list.Reverse();
            return new AccessPath(root, list.ToImmutable());
        }

        public static AccessPath? FromRefArgument(IOperation? operation)
        {
            // also handle `out var decl`
            if (operation is IDeclarationExpressionOperation { Expression: ILocalReferenceOperation { Local: var local } }) {
                return new AccessPath(AccessPathRoot.Local, ImmutableArray.Create<ISymbol>(local));
            } else {
                return FromOperation(operation);
            }
        }

        public override bool Equals(object obj) => obj is AccessPath p && Equals(p);

        public override int GetHashCode()
        {
            int hash = (int)Root;
            foreach (var sym in Symbols) {
                hash ^= SymbolEqualityComparer.Default.GetHashCode(sym);
            }
            return hash;
        }

        public bool Equals(AccessPath other)
        {
            if (Root != other.Root)
                return false;
            if (Symbols.Length != other.Symbols.Length)
                return false;
            return Symbols.Zip(other.Symbols, SymbolEqualityComparer.Default.Equals).All(b => b);
        }

        public override string ToString()
        {
            StringBuilder b = new StringBuilder();
            if (Root == AccessPathRoot.This)
                b.Append("this");
            foreach (var sym in Symbols) {
                if (b.Length > 0)
                    b.Append('.');
                b.Append(sym.Name);
            }
            return b.ToString();
        }
    }
}