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
using System.Diagnostics;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Operations;

namespace ICSharpCode.NullabilityInference
{
    /// <summary>
    /// Walks the syntax tree and constructs NullabilityEdges.
    /// 
    /// This visitor assumes that the NodeBuildingSyntaxVisitor has already run for all syntax trees in the project,
    /// so that the TypeWithNode for any declaration can be looked up in the typeSystem.
    /// This visitor will:
    ///  * For every `TypeSyntax`, retrieve the same `TypeWithNode` as `NodeBuildingSyntaxVisitor` did.
    ///  * For every `ExpressionSyntax`, produce a `TypeWithNode` for the expression's type and nullability.
    ///    * Note: expressions are not handled directly in this class, but instead by the EdgeBuildingOperationVisitor.
    ///  * Other syntactic nodes return `typeSystem.VoidType`
    /// </summary>
    internal class EdgeBuildingSyntaxVisitor : GraphBuildingSyntaxVisitor
    {
        private readonly new TypeSystem typeSystem;
        private readonly TypeSystem.Builder typeSystemBuilder;
        private readonly SyntaxToNodeMapping mapping;
        private readonly EdgeBuildingOperationVisitor operationVisitor;

        public EdgeBuildingSyntaxVisitor(SemanticModel semanticModel, TypeSystem typeSystem, TypeSystem.Builder typeSystemBuilder, SyntaxToNodeMapping mapping, CancellationToken cancellationToken)
            : base(semanticModel, typeSystemBuilder, cancellationToken)
        {
            this.typeSystem = typeSystem;
            this.typeSystemBuilder = typeSystemBuilder;
            this.mapping = mapping;
            this.operationVisitor = new EdgeBuildingOperationVisitor(this, typeSystem, typeSystemBuilder);
        }

        public override TypeWithNode DefaultVisit(SyntaxNode node)
        {
            cancellationToken.ThrowIfCancellationRequested();
            if (node is ExpressionSyntax) {
                if (node is TypeSyntax) {
                    throw new NotImplementedException(node.Kind().ToString());
                } else {
                    return HandleAsOperation(node);
                }
            } else if (node is StatementSyntax) {
                return HandleAsOperation(node);
            }

            foreach (var child in node.ChildNodes()) {
                Visit(child);
            }
            return typeSystem.VoidType;
        }

        private TypeWithNode HandleAsOperation(SyntaxNode node)
        {
            var operation = semanticModel.GetOperation(node, cancellationToken);
            if (operation == null)
                throw new NotSupportedException($"Could not get operation for {node}");
            return operation.Accept(operationVisitor, new EdgeBuildingContext());
        }

        internal bool IsNonNullFlow(SyntaxNode syntax)
        {
            var typeInfo = semanticModel.GetTypeInfo(syntax, cancellationToken);
            return typeInfo.Nullability.FlowState == NullableFlowState.NotNull;
        }

        protected override TypeWithNode HandleTypeName(TypeSyntax node, IEnumerable<TypeSyntax>? typeArguments)
        {
            TypeWithNode[] typeArgs = typeArguments?.Select(s => s.Accept(this)).ToArray() ?? new TypeWithNode[0];
            var symbolInfo = semanticModel.GetSymbolInfo(node, cancellationToken);
            switch (symbolInfo.Symbol) {
                case INamedTypeSymbol ty:
                    var alias = semanticModel.GetAliasInfo(node, cancellationToken);
                    if (alias != null) {
                        typeArgs = typeSystem.GetSymbolType(alias).TypeArguments.ToArray();
                    } else {
                        typeArgs = InheritOuterTypeArguments(typeArgs, ty);
                        Debug.Assert(ty.FullArity() == typeArgs.Length);
                        foreach (var (tp, ta) in ty.FullTypeParameters().Zip(typeArgs)) {
                            if (tp.HasNotNullConstraint) {
                                var edge = typeSystemBuilder.CreateEdge(ta.Node, typeSystem.NonNullNode);
                                edge?.SetLabel("nonnull constraint", node.GetLocation());
                            }
                        }
                    }
                    if (ty.CanBeMadeNullable() && CanBeMadeNullableSyntax(node)) {
                        return new TypeWithNode(ty, mapping[node], typeArgs);
                    } else {
                        return new TypeWithNode(ty, typeSystem.ObliviousNode, typeArgs);
                    }
                case ITypeParameterSymbol tp:
                    if (tp.HasReferenceTypeConstraint && CanBeMadeNullableSyntax(node)) {
                        return new TypeWithNode(tp, mapping[node], typeArgs);
                    } else {
                        return new TypeWithNode(tp, typeSystem.ObliviousNode, typeArgs);
                    }
                case IPointerTypeSymbol pts:
                    return new TypeWithNode(pts, typeSystem.ObliviousNode, typeArgs);
                case IArrayTypeSymbol ats:
                    return new TypeWithNode(ats, mapping[node], typeArgs);
                default:
                    return typeSystem.VoidType;
            }
        }

        public override TypeWithNode VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
        {
            // Can be a type name, e.g. appearing in "Namespace.Type.StaticMethod()".
            var symbolInfo = semanticModel.GetSymbolInfo(node, cancellationToken);
            if (symbolInfo.Symbol is INamedTypeSymbol ty) {
                var typeArgs = CollectTypeArgs(node).Select(Visit).ToArray();
                return new TypeWithNode(ty, typeSystem.ObliviousNode, typeArgs);
            } else {
                return HandleAsOperation(node);
            }
        }

        public override TypeWithNode VisitArrayType(ArrayTypeSyntax node)
        {
            var elementType = node.ElementType.Accept(this);
            // Handle nested arrays
            foreach (var rank in node.RankSpecifiers.Skip(1).Reverse()) {
                // Trying to insert `?` for nested arrays will be tricky,
                // because `int[,][]` with nullable nested arrays has to turn into `int[]?[,]`
                // So for now, just handle nested arrays as oblivious.
                var nestedArrayType = elementType.Type != null ? semanticModel.Compilation.CreateArrayTypeSymbol(elementType.Type) : null;
                elementType = new TypeWithNode(nestedArrayType, typeSystem.ObliviousNode, new[] { elementType });
            }
            var arrayType = elementType.Type != null ? semanticModel.Compilation.CreateArrayTypeSymbol(elementType.Type) : null;
            var nullNode = CanBeMadeNullableSyntax(node) ? mapping[node] : typeSystem.ObliviousNode;
            return new TypeWithNode(arrayType, nullNode, new[] { elementType });
        }

        public override TypeWithNode VisitTupleType(TupleTypeSyntax node)
        {
            var elementTypes = node.Elements.Select(e => e.Type.Accept(this)).ToArray();
            var symbolInfo = semanticModel.GetSymbolInfo(node, cancellationToken);
            return new TypeWithNode(symbolInfo.Symbol as ITypeSymbol, typeSystem.ObliviousNode, elementTypes);
        }

        internal TypeWithNode currentMethodReturnType;

        public override TypeWithNode VisitConstructorDeclaration(ConstructorDeclarationSyntax node)
        {
            var outerMethodReturnType = currentMethodReturnType;
            try {
                currentMethodReturnType = typeSystem.VoidType;
                var operation = semanticModel.GetOperation(node, cancellationToken);
                if (operation == null)
                    throw new NotSupportedException($"Could not get operation for {node}");
                if (node.Initializer?.ThisOrBaseKeyword.Kind() != SyntaxKind.ThisKeyword) {
                    HashSet<ISymbol> initializedSymbols = new HashSet<ISymbol>();
                    foreach (var assgn in operation.DescendantsAndSelf().OfType<ISimpleAssignmentOperation>()) {
                        if (assgn.Target is IFieldReferenceOperation fieldRef) {
                            initializedSymbols.Add(fieldRef.Field);
                        } else if (assgn.Target is IPropertyReferenceOperation propertyRef) {
                            initializedSymbols.Add(propertyRef.Property);
                        } else if (assgn.Target is IEventReferenceOperation eventRef) {
                            initializedSymbols.Add(eventRef.Event);
                        }
                    }
                    if (node.Parent is TypeDeclarationSyntax typeSyntax) {
                        bool isStatic = node.Modifiers.Any(SyntaxKind.StaticKeyword);
                        MarkFieldsAndPropertiesAsNullable(typeSyntax.Members, isStatic, initializedSymbols, node.GetLocation());
                    }
                }
                return operation.Accept(operationVisitor, new EdgeBuildingContext());
            } finally {
                currentMethodReturnType = outerMethodReturnType;
            }
        }

        public override TypeWithNode VisitMethodDeclaration(MethodDeclarationSyntax node)
        {
            return HandleMethodDeclaration(node);
        }

        public override TypeWithNode VisitOperatorDeclaration(OperatorDeclarationSyntax node)
        {
            return HandleMethodDeclaration(node);
        }

        public override TypeWithNode VisitConversionOperatorDeclaration(ConversionOperatorDeclarationSyntax node)
        {
            return HandleMethodDeclaration(node);
        }

        public override TypeWithNode VisitDestructorDeclaration(DestructorDeclarationSyntax node)
        {
            return HandleMethodDeclaration(node);
        }

        private TypeWithNode HandleMethodDeclaration(BaseMethodDeclarationSyntax node)
        { 
            var outerMethodReturnType = currentMethodReturnType;
            try {
                var symbol = semanticModel.GetDeclaredSymbol(node);
                if (symbol != null) {
                    CreateOverrideEdge(symbol, symbol.OverriddenMethod);
                    currentMethodReturnType = typeSystem.GetSymbolType(symbol);
                } else {
                    currentMethodReturnType = typeSystem.VoidType;
                }
                if (node.Body != null || node.ExpressionBody != null) {
                    return HandleAsOperation(node);
                } else {
                    return typeSystem.VoidType;
                }
            } finally {
                currentMethodReturnType = outerMethodReturnType;
            }
        }

        public override TypeWithNode VisitPropertyDeclaration(PropertyDeclarationSyntax node)
        {
            var outerMethodReturnType = currentMethodReturnType;
            try {
                var symbol = semanticModel.GetDeclaredSymbol(node);
                if (symbol != null) {
                    CreateOverrideEdge(symbol, symbol.OverriddenProperty);
                    currentMethodReturnType = typeSystem.GetSymbolType(symbol);
                } else {
                    currentMethodReturnType = typeSystem.VoidType;
                }
                node.AccessorList?.Accept(this);
                node.ExpressionBody?.Accept(this);
                node.Initializer?.Accept(this);
                return typeSystem.VoidType;
            } finally {
                currentMethodReturnType = outerMethodReturnType;
            }
        }

        public override TypeWithNode VisitIndexerDeclaration(IndexerDeclarationSyntax node)
        {
            var outerMethodReturnType = currentMethodReturnType;
            try {
                var symbol = semanticModel.GetDeclaredSymbol(node);
                if (symbol != null) {
                    CreateOverrideEdge(symbol, symbol.OverriddenProperty);
                    currentMethodReturnType = typeSystem.GetSymbolType(symbol);
                } else {
                    currentMethodReturnType = typeSystem.VoidType;
                }
                node.AccessorList?.Accept(this);
                node.ExpressionBody?.Accept(this);
                return typeSystem.VoidType;
            } finally {
                currentMethodReturnType = outerMethodReturnType;
            }
        }

        public override TypeWithNode VisitEventDeclaration(EventDeclarationSyntax node)
        {
            var outerMethodReturnType = currentMethodReturnType;
            try {
                var symbol = semanticModel.GetDeclaredSymbol(node);
                if (symbol != null) {
                    CreateOverrideEdge(symbol, symbol.OverriddenEvent);
                    currentMethodReturnType = typeSystem.GetSymbolType(symbol);
                } else {
                    currentMethodReturnType = typeSystem.VoidType;
                }
                node.AccessorList?.Accept(this);
                return typeSystem.VoidType;
            } finally {
                currentMethodReturnType = outerMethodReturnType;
            }
        }

        public override TypeWithNode VisitBlock(BlockSyntax node)
        {
            return HandleAsOperation(node);
        }

        public override TypeWithNode VisitArrowExpressionClause(ArrowExpressionClauseSyntax node)
        {
            return HandleAsOperation(node);
        }

        public override TypeWithNode VisitEqualsValueClause(EqualsValueClauseSyntax node)
        {
            return HandleAsOperation(node);
        }

        public override TypeWithNode VisitClassDeclaration(ClassDeclarationSyntax node)
        {
            HandleInterfaceImplementations(node);
            HandleLackOfConstructor(node);
            return base.VisitClassDeclaration(node);
        }

        public override TypeWithNode VisitStructDeclaration(StructDeclarationSyntax node)
        {
            HandleInterfaceImplementations(node);
            HandleLackOfConstructor(node);
            return base.VisitStructDeclaration(node);
        }

        private void HandleInterfaceImplementations(TypeDeclarationSyntax node)
        {
            if (!(semanticModel.GetDeclaredSymbol(node) is INamedTypeSymbol classType))
                throw new NotSupportedException("TypeDeclarationSyntax must declare INamedTypedSymbol");
            foreach (var interfaceType in classType.Interfaces) {
                foreach (var interfaceMember in interfaceType.GetMembers()) {
                    var classMember = classType.FindImplementationForInterfaceMember(interfaceMember);
                    if (classMember != null) {
                        CreateOverrideEdge(classMember, interfaceMember);
                    }
                }
            }
        }

        private void CreateOverrideEdge(ISymbol overrideSymbol, ISymbol? baseSymbol)
        {
            if (baseSymbol == null)
                return;
            switch (overrideSymbol, baseSymbol) {
                case (IMethodSymbol overrideMethod, IMethodSymbol baseMethod):
                    var thisType = typeSystem.GetObliviousType(overrideMethod.ContainingType);
                    var overrideReturnType = typeSystem.GetSymbolType(overrideMethod);
                    var baseTypeSubstitution = operationVisitor.SubstitutionForMemberAccess(thisType, baseMethod);
                    var baseReturnType = typeSystem.GetSymbolType(baseMethod.OriginalDefinition);
                    var edge = typeSystemBuilder.CreateTypeEdge(overrideReturnType, baseReturnType, baseTypeSubstitution, VarianceKind.Out);
                    edge?.SetLabel("override", null);

                    foreach (var (overrideParam, baseParam) in overrideMethod.Parameters.Zip(baseMethod.Parameters)) {
                        var overrideParamType = typeSystem.GetSymbolType(overrideParam);
                        var baseParamType = typeSystem.GetSymbolType(baseParam.OriginalDefinition);
                        var variance = overrideParam.RefKind.ToVariance();
                        edge = typeSystemBuilder.CreateTypeEdge(overrideParamType, baseParamType, baseTypeSubstitution, variance);
                        edge?.SetLabel("override", null);
                    }

                    break;
            }
        }

        private void HandleLackOfConstructor(TypeDeclarationSyntax node)
        {
            var hasStaticConstructor = node.Members.Any(m => m is ConstructorDeclarationSyntax ctor && ctor.Modifiers.Any(SyntaxKind.StaticKeyword));
            var hasNonStaticConstructor = node.Members.Any(m => m is ConstructorDeclarationSyntax ctor && !ctor.Modifiers.Any(SyntaxKind.StaticKeyword));
            if (!hasStaticConstructor) {
                // implicit compiler-generated static constructor initializes all static fields to null
                MarkFieldsAndPropertiesAsNullable(node.Members, isStatic: true, new HashSet<ISymbol>(), location: null);
            }
            if (!hasNonStaticConstructor) {
                // implicit compiler-generated constructor initializes all instance fields to null
                MarkFieldsAndPropertiesAsNullable(node.Members, isStatic: false, new HashSet<ISymbol>(), location: null);
            }
        }

        private void MarkFieldsAndPropertiesAsNullable(SyntaxList<MemberDeclarationSyntax> members, bool isStatic, HashSet<ISymbol> initializedMembers, Location? location)
        {
            foreach (var member in members) {
                if (member.Modifiers.Any(SyntaxKind.StaticKeyword) != isStatic)
                    continue;
                if (member is FieldDeclarationSyntax fieldDecl) {
                    MarkField(fieldDecl.Declaration.Variables);
                } else if (member is EventFieldDeclarationSyntax eventDecl) {
                    MarkField(eventDecl.Declaration.Variables);
                } else if (member is PropertyDeclarationSyntax { Initializer: null } propertyDecl && propertyDecl.IsAutoProperty()) {
                    var property = semanticModel.GetDeclaredSymbol(propertyDecl, cancellationToken);
                    if (property != null && !initializedMembers.Contains(property)) {
                        var symbolType = typeSystem.GetSymbolType(property);
                        var edge = typeSystemBuilder.CreateEdge(typeSystem.NullableNode, symbolType.Node);
                        edge?.SetLabel("uninit", location);
                    }
                }
            }

            void MarkField(SeparatedSyntaxList<VariableDeclaratorSyntax> variableDeclarators)
            {
                foreach (var v in variableDeclarators) {
                    if (v.Initializer == null) {
                        var field = semanticModel.GetDeclaredSymbol(v, cancellationToken);
                        if (field != null && !initializedMembers.Contains(field)) {
                            var symbolType = typeSystem.GetSymbolType(field);
                            var edge = typeSystemBuilder.CreateEdge(typeSystem.NullableNode, symbolType.Node);
                            edge?.SetLabel("uninit", location);
                        }
                    }
                }
            }
        }
    }
}
