using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Text;
using Microsoft.CodeAnalysis;

namespace ICSharpCode.NullabilityInference
{
    /// <summary>
    /// Represents the flow-state at a given point in the program.
    /// </summary>
    internal sealed class FlowState
    {
        internal readonly struct Snapshot
        {
            internal readonly bool Unreachable;
            internal readonly PathNode ThisPath;
            internal readonly ImmutableDictionary<ISymbol, PathNode> Locals;

            public Snapshot(PathNode thisPath, ImmutableDictionary<ISymbol, PathNode> locals, bool unreachable)
            {
                this.ThisPath = thisPath;
                this.Locals = locals;
                this.Unreachable = unreachable;
            }

            internal Snapshot? WithUnreachable()
            {
                return new Snapshot(ThisPath, Locals, unreachable: true);
            }
        }

        internal readonly struct PathNode
        {
            public readonly NullabilityNode Nullability;
            public readonly ImmutableDictionary<ISymbol, PathNode> Members;

            public PathNode(NullabilityNode nullability, ImmutableDictionary<ISymbol, PathNode> members)
            {
                this.Nullability = nullability;
                this.Members = members;
            }
        }

        private static readonly ImmutableDictionary<ISymbol, PathNode> emptyMembers = ImmutableDictionary.Create<ISymbol, PathNode>();

        private readonly TypeSystem typeSystem;

        private bool unreachable;
        private PathNode thisPath;
        private ImmutableDictionary<ISymbol, PathNode>.Builder locals = ImmutableDictionary.CreateBuilder<ISymbol, PathNode>();

        public FlowState(TypeSystem typeSystem)
        {
            this.typeSystem = typeSystem;
            Clear();
        }

        public void Clear()
        {
            unreachable = false;
            thisPath = new PathNode(typeSystem.NonNullNode, emptyMembers);
            locals.Clear();
        }

        public void MakeUnreachable()
        {
            unreachable = true;
        }

        public Snapshot SaveSnapshot()
        {
            return new Snapshot(thisPath, locals.ToImmutable(), unreachable);
        }

        public void RestoreSnapshot(Snapshot snapshot)
        {
            this.unreachable = snapshot.Unreachable;
            this.thisPath = snapshot.ThisPath;
            this.locals = snapshot.Locals.ToBuilder();
        }

        public void JoinWith(Snapshot snapshot, TypeSystem.Builder tsBuilder, EdgeLabel edgeLabel)
        {
            if (unreachable) {
                RestoreSnapshot(snapshot);
                return;
            } else if (snapshot.Unreachable) {
                return; // no-op
            }
            Debug.Assert(!unreachable && !snapshot.Unreachable);
            thisPath = Join(thisPath, snapshot.ThisPath);
            locals = JoinDict(locals.ToImmutable(), snapshot.Locals);

            ImmutableDictionary<ISymbol, PathNode>.Builder JoinDict(ImmutableDictionary<ISymbol, PathNode> a, ImmutableDictionary<ISymbol, PathNode> b)
            {
                ImmutableDictionary<ISymbol, PathNode>.Builder newDict = emptyMembers.ToBuilder();
                foreach (var (local, pathFromSnapshot) in b) {
                    if (a.TryGetValue(local, out var pathFromThis)) {
                        newDict[local] = Join(pathFromThis, pathFromSnapshot);
                    }
                }
                return newDict;
            }

            PathNode Join(PathNode a, PathNode b)
            {
                var newMembers = JoinDict(a.Members, b.Members);
                var newNullability = tsBuilder.Join(a.Nullability, b.Nullability, edgeLabel);
                return new PathNode(newNullability, newMembers.ToImmutable());
            }
        }

        public void SetNode(AccessPath path, NullabilityNode newNode, bool clearMembers)
        {
            switch (path.Root) {
                case AccessPathRoot.This:
                    thisPath = Visit(thisPath, 0);
                    break;
                case AccessPathRoot.Local:
                    if (!locals.TryGetValue(path.Symbols[0], out var localPathNode)) {
                        localPathNode = new PathNode(typeSystem.NonNullNode, emptyMembers);
                    }
                    localPathNode = Visit(localPathNode, 1);
                    locals[path.Symbols[0]] = localPathNode;
                    break;
                default:
                    throw new NotSupportedException();
            }

            PathNode Visit(PathNode input, int index)
            {
                if (index == path.Symbols.Length) {
                    if (clearMembers) {
                        return new PathNode(newNode, emptyMembers);
                    } else {
                        return new PathNode(newNode, input.Members);
                    }
                }
                var member = path.Symbols[index];
                if (!input.Members.TryGetValue(member, out var childNode)) {
                    childNode = new PathNode(typeSystem.NonNullNode, emptyMembers);
                }
                childNode = Visit(childNode, index + 1);
                return new PathNode(typeSystem.NonNullNode, input.Members.SetItem(member, childNode));
            }
        }

        public bool TryGetNode(AccessPath path, [NotNullWhen(true)] out NullabilityNode? flowNode)
        {
            flowNode = null;
            int index;
            PathNode node;
            switch (path.Root) {
                case AccessPathRoot.This:
                    index = 0;
                    node = thisPath;
                    break;
                case AccessPathRoot.Local:
                    if (!locals.TryGetValue(path.Symbols[0], out node)) {
                        return false;
                    }
                    index = 1;
                    break;
                default:
                    throw new NotSupportedException();
            }
            for (; index < path.Symbols.Length; index++) {
                if (!node.Members.TryGetValue(path.Symbols[index], out node)) {
                    return false;
                }
            }
            flowNode = node.Nullability;
            return true;
        }
    }
}
