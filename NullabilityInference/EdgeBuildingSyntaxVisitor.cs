﻿// Copyright (c) 2020 Daniel Grunwald
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
            return operation.Accept(operationVisitor, EdgeBuildingContext.Normal);
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
                                typeSystemBuilder.CreateEdge(ta.Node, typeSystem.NonNullNode, new EdgeLabel("nonnull constraint", node));
                            }
                        }
                    }
                    if (ty.CanBeMadeNullable() && CanBeMadeNullableSyntax(node)) {
                        return new TypeWithNode(ty, mapping[node], typeArgs);
                    } else {
                        return new TypeWithNode(ty, typeSystem.ObliviousNode, typeArgs);
                    }
                case ITypeParameterSymbol tp:
                    if (tp.CanBeMadeNullable() && CanBeMadeNullableSyntax(node)) {
                        return new TypeWithNode(tp, mapping[node], typeArgs);
                    } else {
                        return new TypeWithNode(tp, typeSystem.ObliviousNode, typeArgs);
                    }
                case IPointerTypeSymbol pts:
                    return new TypeWithNode(pts, typeSystem.ObliviousNode, typeArgs);
                case IArrayTypeSymbol ats:
                    return new TypeWithNode(ats, mapping[node], typeArgs);
                case IDynamicTypeSymbol dynamicType:
                    Debug.Assert(dynamicType.CanBeMadeNullable());
                    if (CanBeMadeNullableSyntax(node)) {
                        return new TypeWithNode(dynamicType, mapping[node], typeArgs);
                    } else {
                        return new TypeWithNode(dynamicType, typeSystem.ObliviousNode, typeArgs);
                    }
                default:
                    return typeSystem.VoidType;
            }
        }

        public override TypeWithNode VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
        {
            // Can be a type name, e.g. appearing in "Namespace.Type.StaticMethod()".
            var symbolInfo = semanticModel.GetSymbolInfo(node, cancellationToken);
            if (symbolInfo.Symbol is INamedTypeSymbol ty) {
                TypeWithNode[] typeArgs = CollectTypeArgs(node).Select(Visit).ToArray();
                return new TypeWithNode(ty, typeSystem.ObliviousNode, typeArgs);
            } else {
                return HandleAsOperation(node);
            }
        }

        protected override NullabilityNode GetMappedNode(TypeSyntax syntax)
        {
            return mapping[syntax];
        }

        internal IMethodSymbol? currentMethod;
        internal TypeWithNode currentMethodReturnType;

        public override TypeWithNode VisitConstructorDeclaration(ConstructorDeclarationSyntax node)
        {
            using var outerMethod = SaveCurrentMethod();
            currentMethod = semanticModel.GetDeclaredSymbol(node, cancellationToken);
            currentMethodReturnType = typeSystem.VoidType;
            var operation = semanticModel.GetOperation(node, cancellationToken);
            if (operation == null)
                throw new NotSupportedException($"Could not get operation for {node}");
            if (node.Initializer?.ThisOrBaseKeyword.Kind() != SyntaxKind.ThisKeyword) {
                HashSet<ISymbol> initializedSymbols = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
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
                    MarkFieldsAndPropertiesAsNullable(typeSyntax.Members, isStatic, initializedSymbols, new EdgeLabel("uninit", node));
                }
            }
            return operation.Accept(operationVisitor, new EdgeBuildingContext());
        }

        internal void HandleCref(NameMemberCrefSyntax cref)
        {
            var symbolInfo = semanticModel.GetSymbolInfo(cref, cancellationToken);
            if (symbolInfo.Symbol is IMethodSymbol method && cref.Parameters != null) {
                foreach (var (crefParam, param) in cref.Parameters.Parameters.Zip(method.Parameters)) {
                    var crefType = crefParam.Type.Accept(this);
                    var symbolType = typeSystem.GetSymbolType(param, ignoreAttributes: true);
                    // create bidirectional edge between both
                    typeSystemBuilder.CreateTypeEdge(crefType, symbolType, null, VarianceKind.None, new EdgeLabel("cref", crefParam));
                }
            }
        }

        public override TypeWithNode VisitMethodDeclaration(MethodDeclarationSyntax node)
        {
            node.ExplicitInterfaceSpecifier?.Accept(this);
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

        internal IDisposable SaveCurrentMethod()
        {
            var outerMethod = currentMethod;
            var outerMethodReturnType = currentMethodReturnType;
            return new CallbackOnDispose(delegate {
                currentMethod = outerMethod;
                currentMethodReturnType = outerMethodReturnType;
            });
        }

        private TypeWithNode HandleMethodDeclaration(BaseMethodDeclarationSyntax node)
        {
            using var outerMethod = SaveCurrentMethod();
            currentMethod = semanticModel.GetDeclaredSymbol(node);
            if (currentMethod != null) {
                CreateOverrideEdge(currentMethod, currentMethod.OverriddenMethod);
                currentMethodReturnType = GetMethodReturnSymbol(currentMethod);
            } else {
                currentMethodReturnType = typeSystem.VoidType;
            }
            if (node.Body != null || node.ExpressionBody != null) {
                return HandleAsOperation(node);
            } else {
                return typeSystem.VoidType;
            }
        }

        internal TypeWithNode GetMethodReturnSymbol(IMethodSymbol method)
        {
            var returnType = typeSystem.GetSymbolType(method);
            if (method.IsAsync) {
                returnType = ExtractTaskReturnType(returnType);
            }
            return returnType;
        }

        internal TypeWithNode ExtractTaskReturnType(TypeWithNode taskType)
        {
            // See also: EffectiveReturnType() extension method
            if (taskType.TypeArguments.Count == 0) {
                return typeSystem.VoidType;
            } else {
                return taskType.TypeArguments.Single();
            }
        }

        public override TypeWithNode VisitPropertyDeclaration(PropertyDeclarationSyntax node)
        {
            node.ExplicitInterfaceSpecifier?.Accept(this);
            using var outerMethod = SaveCurrentMethod();
            var symbol = semanticModel.GetDeclaredSymbol(node);
            if (symbol != null) {
                CreateOverrideEdge(symbol, symbol.OverriddenProperty);
                currentMethodReturnType = typeSystem.GetSymbolType(symbol);
            } else {
                currentMethodReturnType = typeSystem.VoidType;
            }
            currentMethod = null;
            node.AccessorList?.Accept(this);
            node.ExpressionBody?.Accept(this);
            node.Initializer?.Accept(this);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitIndexerDeclaration(IndexerDeclarationSyntax node)
        {
            node.ExplicitInterfaceSpecifier?.Accept(this);
            using var outerMethod = SaveCurrentMethod();
            var symbol = semanticModel.GetDeclaredSymbol(node);
            if (symbol != null) {
                CreateOverrideEdge(symbol, symbol.OverriddenProperty);
                currentMethodReturnType = typeSystem.GetSymbolType(symbol);
            } else {
                currentMethodReturnType = typeSystem.VoidType;
            }
            currentMethod = null;
            node.AccessorList?.Accept(this);
            node.ExpressionBody?.Accept(this);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitEventDeclaration(EventDeclarationSyntax node)
        {
            node.ExplicitInterfaceSpecifier?.Accept(this);
            using var outerMethod = SaveCurrentMethod();
            var symbol = semanticModel.GetDeclaredSymbol(node);
            if (symbol != null) {
                CreateOverrideEdge(symbol, symbol.OverriddenEvent);
                currentMethodReturnType = typeSystem.GetSymbolType(symbol);
            } else {
                currentMethodReturnType = typeSystem.VoidType;
            }
            currentMethod = null;
            node.AccessorList?.Accept(this);
            return typeSystem.VoidType;
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
            HandleLackOfConstructor(node, skipInstanceCtor: false);
            return base.VisitClassDeclaration(node);
        }

        public override TypeWithNode VisitStructDeclaration(StructDeclarationSyntax node)
        {
            HandleInterfaceImplementations(node);
            HandleLackOfConstructor(node, skipInstanceCtor: true);
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
                    typeSystemBuilder.CreateTypeEdge(overrideReturnType, baseReturnType, baseTypeSubstitution, VarianceKind.Out, new EdgeLabel("override"));

                    foreach (var (overrideParam, baseParam) in overrideMethod.Parameters.Zip(baseMethod.Parameters)) {
                        var overrideParamType = typeSystem.GetSymbolType(overrideParam);
                        var baseParamType = typeSystem.GetSymbolType(baseParam.OriginalDefinition);
                        var variance = overrideParam.RefKind.ToVariance();
                        typeSystemBuilder.CreateTypeEdge(overrideParamType, baseParamType, baseTypeSubstitution, variance, new EdgeLabel("override"));
                    }

                    break;
            }
        }

        public override TypeWithNode VisitExplicitInterfaceSpecifier(ExplicitInterfaceSpecifierSyntax node)
        {
            // For something like "IEnumerator<string?> IEnumerable<string?>.GetEnumerator()",
            // the return type is already handled by HandleInterfaceImplementations/CreateOverrideEdge
            // and effectively connected with the node for the current classes' base list.

            // Here we do the same for the occurrence in the explicit interface specifier.
            var interfaceType = node.Name.Accept(this);
            var symbol = semanticModel.GetEnclosingSymbol(node.SpanStart, cancellationToken);
            if (symbol is INamedTypeSymbol containingType && interfaceType.Type is INamedTypeSymbol implementedInterface) {
                var thisType = typeSystem.GetObliviousType(containingType);
                var implementedInterfaceType = typeSystem.GetBaseType(thisType, implementedInterface);
                if (implementedInterfaceType != null) {
                    typeSystemBuilder.CreateTypeEdge(interfaceType, implementedInterfaceType.Value, null, VarianceKind.None, new EdgeLabel("explicit interface impl", node));
                }
            }
            return typeSystem.VoidType;
        }

        private void HandleLackOfConstructor(TypeDeclarationSyntax node, bool skipInstanceCtor)
        {
            var hasStaticConstructor = node.Members.Any(m => m is ConstructorDeclarationSyntax ctor && ctor.Modifiers.Any(SyntaxKind.StaticKeyword));
            var hasNonStaticConstructor = node.Members.Any(m => m is ConstructorDeclarationSyntax ctor && !ctor.Modifiers.Any(SyntaxKind.StaticKeyword));
            if (!hasStaticConstructor) {
                // implicit compiler-generated static constructor initializes all static fields to null
                MarkFieldsAndPropertiesAsNullable(node.Members, isStatic: true, new HashSet<ISymbol>(SymbolEqualityComparer.Default), new EdgeLabel("uninit"));
            }
            if (!hasNonStaticConstructor && !skipInstanceCtor) {
                // implicit compiler-generated constructor initializes all instance fields to null
                MarkFieldsAndPropertiesAsNullable(node.Members, isStatic: false, new HashSet<ISymbol>(SymbolEqualityComparer.Default), new EdgeLabel("uninit"));
            }
        }

        private void MarkFieldsAndPropertiesAsNullable(SyntaxList<MemberDeclarationSyntax> members, bool isStatic, HashSet<ISymbol> initializedMembers, EdgeLabel label)
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
                        typeSystemBuilder.CreateEdge(typeSystem.NullableNode, symbolType.Node, label);
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
                            typeSystemBuilder.CreateEdge(typeSystem.NullableNode, symbolType.Node, label);
                        }
                    }
                }
            }
        }

        internal bool IsReducedExtensionMethodCall(InvocationExpressionSyntax invocation)
        {
            var symbolInfo = semanticModel.GetSymbolInfo(invocation, cancellationToken);
            return symbolInfo.Symbol is IMethodSymbol { MethodKind: MethodKind.ReducedExtension };
        }

        public override TypeWithNode VisitVariableDeclarator(VariableDeclaratorSyntax node)
        {
            // skip node.ArgumentList (array size of fixed field), it doesn't have an associated operation
            node.Initializer?.Accept(this);
            return typeSystem.VoidType;
        }
    }
}
