// Copyright (c) 2020 Daniel Grunwald

using System.Collections.Generic;
using System.Diagnostics;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace NullabilityInference
{
    /// <summary>
    /// Maps from syntax nodes to nullability nodes.
    /// There is one mapping per SyntaxTree.
    /// </summary>
    internal class SyntaxToNodeMapping
    {
        private readonly Dictionary<TypeSyntax, SyntacticNullabilityNode> nodes = new Dictionary<TypeSyntax, SyntacticNullabilityNode>();

        public SyntacticNullabilityNode this[TypeSyntax syntax] => nodes[syntax];

        public IEnumerable<SyntacticNullabilityNode> Nodes => nodes.Values;

        public SyntacticNullabilityNode CreateNewNode(TypeSyntax syntax)
        {
            var nullNode = new SyntacticNullabilityNode(syntax, nodes.Count + 1);
            nodes.Add(syntax, nullNode);
            return nullNode;
        }
    }

    /// <summary>
    /// Creates NullabilityNodes for types that appear in the syntax tree.
    /// 
    /// We create a NullabilityNode for each syntactic occurrence of a reference type,
    /// as those are the places where we could insert `?`.
    /// This visitor maps every `TypeSyntax` to a `TypeWithNode`.
    /// All other kinds of syntax nodes just return `typeSystem.VoidType` (even expressions that don't produce `void`
    /// -- only the later EdgeBuildingSyntaxVisitor uses accurate types for expressions).
    /// For declarations, this visitor registers the `TypeWithNode` with our `typeSystem` instance.
    /// </summary>
    internal sealed class NodeBuildingSyntaxVisitor : GraphBuildingSyntaxVisitor
    {
        private readonly SemanticModel semanticModel;
        private readonly TypeSystem.Builder typeSystem;
        private readonly CancellationToken cancellationToken;

        /// <summary>
        /// Gets the resulting SyntaxToNodeMapping.
        /// </summary>
        public SyntaxToNodeMapping Mapping { get; } = new SyntaxToNodeMapping();

        public NodeBuildingSyntaxVisitor(SemanticModel semanticModel, TypeSystem.Builder typeSystem, CancellationToken cancellationToken)
        {
            this.semanticModel = semanticModel;
            this.typeSystem = typeSystem;
            this.cancellationToken = cancellationToken;
        }

        public override TypeWithNode DefaultVisit(SyntaxNode node)
        {
            cancellationToken.ThrowIfCancellationRequested();
            Debug.Assert(!(node is TypeSyntax));
            foreach (var child in node.ChildNodes()) {
                Visit(child);
            }
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitIdentifierName(IdentifierNameSyntax node)
        {
            return HandleTypeName(node);
        }

        public override TypeWithNode VisitPredefinedType(PredefinedTypeSyntax node)
        {
            return HandleTypeName(node);
        }

        private TypeWithNode HandleTypeName(TypeSyntax node)
        {
            var symbolInfo = semanticModel.GetSymbolInfo(node, cancellationToken);
            if (symbolInfo.Symbol is ITypeSymbol ty) {
                if (ty.IsValueType) {
                    return new TypeWithNode(ty, typeSystem.ObliviousNode);
                }
                return new TypeWithNode(ty, Mapping.CreateNewNode(node));
            }
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitVariableDeclaration(VariableDeclarationSyntax node)
        {
            var type = node.Type.Accept(this);
            foreach (var v in node.Variables) {
                v.Accept(this);
                var symbol = semanticModel.GetDeclaredSymbol(v, cancellationToken);
                if (symbol != null) {
                    typeSystem.AddSymbolType(symbol, type);
                }
            }
            return type;
        }

        public override TypeWithNode VisitParameter(ParameterSyntax node)
        {
            if (node.Type != null) {
                var type = node.Type.Accept(this);
                var symbol = semanticModel.GetDeclaredSymbol(node, cancellationToken);
                if (symbol != null) {
                    typeSystem.AddSymbolType(symbol, type);
                }
            }
            node.Default?.Accept(this);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitMethodDeclaration(MethodDeclarationSyntax node)
        {
            var returnType = node.ReturnType.Accept(this);
            var symbol = semanticModel.GetDeclaredSymbol(node, cancellationToken);
            if (symbol != null) {
                typeSystem.AddSymbolType(symbol, returnType);
            }
            foreach (var child in node.ChildNodes()) {
                if (child != node.ReturnType)
                    Visit(child);
            }
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitPropertyDeclaration(PropertyDeclarationSyntax node)
        {
            var returnType = node.Type.Accept(this);
            var symbol = semanticModel.GetDeclaredSymbol(node, cancellationToken);
            if (symbol != null) {
                typeSystem.AddSymbolType(symbol, returnType);
            }
            foreach (var child in node.ChildNodes()) {
                if (child != node.Type)
                    Visit(child);
            }
            return typeSystem.VoidType;
        }
    }
}
