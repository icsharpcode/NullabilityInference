using System;
using System.Collections.Generic;
using System.Security.Cryptography;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace NullabilityInference
{
    public sealed class TypeSystem
    {
        public NullabilityNode NullableNode { get; } = new SpecialNullabilityNode(NullType.Nullable);
        public NullabilityNode NonNullNode { get; } = new SpecialNullabilityNode(NullType.NonNull);
        public NullabilityNode ObliviousNode { get; } = new SpecialNullabilityNode(NullType.Oblivious);

        private readonly Dictionary<SyntaxTree, SyntaxToNodeMapping> syntaxMapping = new Dictionary<SyntaxTree, SyntaxToNodeMapping>();
        private readonly Dictionary<ISymbol, TypeWithNode> symbolType = new Dictionary<ISymbol, TypeWithNode>();


        private readonly INamedTypeSymbol voidType;
        public TypeWithNode VoidType => new TypeWithNode(voidType, ObliviousNode);

        public TypeSystem(Compilation compilation)
        {
            this.voidType = compilation.GetSpecialType(SpecialType.System_Void);
        }

        public TypeWithNode GetSymbolType(ISymbol symbol)
        {
            return symbolType[symbol];
        }

        internal TypeWithNode FromType(ITypeSymbol? type, NullableAnnotation nullability)
        {
            return new TypeWithNode(type ?? voidType, nullability switch
            {
                NullableAnnotation.Annotated => NullableNode,
                NullableAnnotation.NotAnnotated => NonNullNode,
                _ => ObliviousNode,
            });
        }

        public IEnumerable<NullabilityNode> AllNodes {
            get {
                yield return NullableNode;
                yield return NonNullNode;
                yield return ObliviousNode;
                foreach (var mapping in syntaxMapping.Values) {
                    foreach (var node in mapping.Nodes) {
                        yield return node;
                    }
                }
            }
        }

        internal IEnumerable<NullabilityNode> ParameterNodes {
            get {
                foreach (var (sym, type) in symbolType) {
                    if (sym.Kind == SymbolKind.Parameter) {
                        yield return type.Node;
                    }
                }
            }
        }

        internal void RegisterNodes(SyntaxTree syntaxTree, SyntaxToNodeMapping mapping)
        {
            syntaxMapping.Add(syntaxTree, mapping);
        }

        internal SyntaxToNodeMapping GetMapping(SyntaxTree syntaxTree)
        {
            return syntaxMapping[syntaxTree];
        }

        /// <summary>
        /// Caches additions to the type system, actually adding them when Flush() is called.
        /// </summary>
        /// <remarks>
        /// Neither the type-system nor the builder is thread-safe.
        /// However, multiple builders can be used concurrently
        /// </remarks>
        internal class Builder
        {
            public readonly NullabilityNode NullableNode;
            public readonly NullabilityNode NonNullNode;
            public readonly NullabilityNode ObliviousNode;
            public readonly TypeWithNode VoidType;

            public Builder(TypeSystem typeSystem)
            {
                // Don't store the typeSystem is this; we may not access it outside of Flush().
                this.NullableNode = typeSystem.NullableNode;
                this.NonNullNode = typeSystem.NonNullNode;
                this.ObliviousNode = typeSystem.ObliviousNode;
                this.VoidType = typeSystem.VoidType;
            }

            public void AddSymbolType(ISymbol symbol, TypeWithNode type)
            {
                type.Node.AddNameFromSymbol(symbol);
                AddAction(ts => ts.symbolType.Add(symbol, type));
            }

            private readonly List<Action<TypeSystem>> cachedActions = new List<Action<TypeSystem>>();

            private void AddAction(Action<TypeSystem> action)
            {
                cachedActions.Add(action);
            }

            public void Flush(TypeSystem typeSystem)
            {
                foreach (var action in cachedActions) {
                    action(typeSystem);
                }
                cachedActions.Clear();
            }
        }
    }
}
