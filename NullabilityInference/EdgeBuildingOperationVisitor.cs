﻿// Copyright (c) 2020 Daniel Grunwald

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Operations;

namespace NullabilityInference
{
    internal struct EdgeBuildingContext { }

    internal class EdgeBuildingOperationVisitor : OperationVisitor<EdgeBuildingContext, TypeWithNode>
    {
        private readonly EdgeBuildingSyntaxVisitor syntaxVisitor;
        private readonly TypeSystem typeSystem;
        private readonly TypeSystem.Builder tsBuilder;

        internal EdgeBuildingOperationVisitor(EdgeBuildingSyntaxVisitor syntaxVisitor, TypeSystem typeSystem, TypeSystem.Builder tsBuilder)
        {
            this.syntaxVisitor = syntaxVisitor;
            this.typeSystem = typeSystem;
            this.tsBuilder = tsBuilder;
        }

        public override TypeWithNode DefaultVisit(IOperation operation, EdgeBuildingContext argument)
        {
            throw new NotImplementedException(operation.Kind.ToString());
        }

        public override TypeWithNode VisitMethodBodyOperation(IMethodBodyOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Children)
                child.Accept(this, argument);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitConstructorBodyOperation(IConstructorBodyOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Children)
                child.Accept(this, argument);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitBlock(IBlockOperation operation, EdgeBuildingContext argument)
        {
            int oldVariableCount = localVariables.Count;
            foreach (var child in operation.Operations)
                child.Accept(this, argument);
            // clean up all variables added within the block
            while (localVariables.Count > oldVariableCount) {
                localVarTypes.Remove(localVariables[localVariables.Count - 1]);
                localVariables.RemoveAt(localVariables.Count - 1);
            }
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitForLoop(IForLoopOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Children)
                child.Accept(this, argument);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitWhileLoop(IWhileLoopOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Children)
                child.Accept(this, argument);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitForEachLoop(IForEachLoopOperation operation, EdgeBuildingContext argument)
        {
            TypeWithNode collection;
            if (operation.Collection is IConversionOperation { IsImplicit: true, Conversion: { IsReference: true }, Operand: { Type: { TypeKind: TypeKind.Array } } arrayOperand }) {
                // special case: the operation tree pretends that non-generic IEnumerable is used when iterating over arrays,
                // but that would cause us to lose information about the element's nullability.
                collection = arrayOperand.Accept(this, argument);
            } else {
                collection = operation.Collection.Accept(this, argument);
            }
            Dereference(collection, operation);
            var loopVariable = operation.LoopControlVariable.Accept(this, argument);
            var loopInfo = syntaxVisitor.semanticModel.GetForEachStatementInfo((CommonForEachStatementSyntax)operation.Syntax);
            // HACK: assume we're only iterating over types generic in the iteration element
            tsBuilder.CreateAssignmentEdge(collection.TypeArguments.Single(), loopVariable);
            operation.Body.Accept(this, argument);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitBranch(IBranchOperation operation, EdgeBuildingContext argument)
        {
            // goto / break / continue
            foreach (var child in operation.Children)
                child.Accept(this, argument);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitUsing(IUsingOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Children)
                child.Accept(this, argument);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitEmpty(IEmptyOperation operation, EdgeBuildingContext argument)
        {
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitExpressionStatement(IExpressionStatementOperation operation, EdgeBuildingContext argument)
        {
            operation.Operation.Accept(this, argument);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitReturn(IReturnOperation operation, EdgeBuildingContext argument)
        {
            if (operation.ReturnedValue != null) {
                var returnVal = operation.ReturnedValue.Accept(this, argument);
                var returnType = syntaxVisitor.currentMethodReturnType;
                if (operation.Kind == OperationKind.YieldReturn) {
                    if (returnType.TypeArguments.Count == 0) {
                        // returning non-generic enumerable
                        return typeSystem.VoidType;
                    }
                    returnType = returnType.TypeArguments.Single();
                }
                var edge = tsBuilder.CreateAssignmentEdge(
                    source: returnVal,
                    target: returnType);
                edge?.SetLabel("return", operation.Syntax.GetLocation());
            }
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitConditional(IConditionalOperation operation, EdgeBuildingContext argument)
        {
            operation.Condition.Accept(this, argument);
            var whenTrue = operation.WhenTrue.Accept(this, argument);
            var whenFalse = operation.WhenFalse?.Accept(this, argument);
            Debug.Assert(whenTrue.Node.NullType == NullType.Oblivious && whenTrue.TypeArguments.Count == 0);
            Debug.Assert(!whenFalse.HasValue || whenFalse.Value.Node.NullType == NullType.Oblivious);
            return whenTrue;
        }

        public override TypeWithNode VisitUnaryOperator(IUnaryOperation operation, EdgeBuildingContext argument)
        {
            if (operation.OperatorMethod != null)
                throw new NotImplementedException("Overloaded operator");
            operation.Operand.Accept(this, argument);
            return typeSystem.GetObliviousType(operation.Type);
        }

        public override TypeWithNode VisitIncrementOrDecrement(IIncrementOrDecrementOperation operation, EdgeBuildingContext argument)
        {
            if (operation.OperatorMethod != null)
                throw new NotImplementedException("Overloaded operator");
            operation.Target.Accept(this, argument);
            return typeSystem.GetObliviousType(operation.Type);
        }

        public override TypeWithNode VisitBinaryOperator(IBinaryOperation operation, EdgeBuildingContext argument)
        {
            if (operation.OperatorMethod != null)
                throw new NotImplementedException("Overloaded operator");
            var lhs = operation.LeftOperand.Accept(this, argument);
            var rhs = operation.RightOperand.Accept(this, argument);
            if (operation.OperatorKind == BinaryOperatorKind.NotEquals || operation.OperatorKind == BinaryOperatorKind.Equals) {
                // check for 'Debug.Assert(x != null);'
                if (IsNullLiteral(operation.RightOperand)) {
                    HandleNullAssert(operation, lhs, valueOnNull: operation.OperatorKind == BinaryOperatorKind.Equals);
                } else if (IsNullLiteral(operation.LeftOperand)) {
                    HandleNullAssert(operation, rhs, valueOnNull: operation.OperatorKind == BinaryOperatorKind.Equals);
                }
            }
            return typeSystem.GetObliviousType(operation.Type);
        }

        private bool IsNullLiteral(IOperation operation)
        {
            return operation.ConstantValue is { HasValue: true, Value: null };
        }

        private void HandleNullAssert(IOperation operation, TypeWithNode testedNode, bool valueOnNull)
        {
            // in 'Debug.Assert(x != null)', operation is `x != null`, testedNode is the type of x and valueOnNull is false.
            while (true) {
                if (!valueOnNull && operation.Parent is IBinaryOperation { OperatorKind: BinaryOperatorKind.ConditionalAnd, OperatorMethod: null } binaryAnd) {
                    operation = binaryAnd;
                } else if (valueOnNull && operation.Parent is IBinaryOperation { OperatorKind: BinaryOperatorKind.ConditionalOr, OperatorMethod: null } binaryOr) {
                    operation = binaryOr;
                } else if (operation.Parent is IUnaryOperation { OperatorKind: UnaryOperatorKind.Not, OperatorMethod: null } unaryNot) {
                    operation = unaryNot;
                    valueOnNull = !valueOnNull;
                } else {
                    break;
                }
            }
            if (operation.Parent is IArgumentOperation argument) {
                foreach (var attr in argument.Parameter.GetAttributes()) {
                    if (attr.ConstructorArguments.Length == 1 && attr.AttributeClass.GetFullName() == "System.Diagnostics.CodeAnalysis.DoesNotReturnIfAttribute") {
                        if (attr.ConstructorArguments.Single().Value is bool val && val == valueOnNull) {
                            tsBuilder.CreateEdge(testedNode.Node, typeSystem.NonNullNode)?.SetLabel("Assert", operation.Syntax?.GetLocation());
                            break;
                        }
                    }
                }
            }
        }

        public override TypeWithNode VisitCompoundAssignment(ICompoundAssignmentOperation operation, EdgeBuildingContext argument)
        {
            if (operation.OperatorMethod != null)
                throw new NotImplementedException("Overloaded operator");
            var lhs = operation.Target.Accept(this, argument);
            var rhs = operation.Value.Accept(this, argument);
            return typeSystem.GetObliviousType(operation.Type);
        }

        public override TypeWithNode VisitCoalesce(ICoalesceOperation operation, EdgeBuildingContext argument)
        {
            var lhs = operation.Value.Accept(this, argument);
            var rhs = operation.WhenNull.Accept(this, argument);
            // TODO: handle generics
            return rhs;
        }

        public override TypeWithNode VisitThrow(IThrowOperation operation, EdgeBuildingContext argument)
        {
            var exception = operation.Exception.Accept(this, argument);
            Dereference(exception, operation);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitLocalReference(ILocalReferenceOperation operation, EdgeBuildingContext argument)
        {
            if (!localVarTypes.TryGetValue(operation.Local, out TypeWithNode variableType)) {
                variableType = typeSystem.GetSymbolType(operation.Local);
            }
            if (syntaxVisitor.IsNonNullFlow(operation.Syntax)) {
                variableType = variableType.WithNode(typeSystem.NonNullNode);
            }
            return variableType;
        }

        public override TypeWithNode VisitParameterReference(IParameterReferenceOperation operation, EdgeBuildingContext argument)
        {
            if (!localVarTypes.TryGetValue(operation.Parameter, out TypeWithNode parameterType)) {
                parameterType = typeSystem.GetSymbolType(operation.Parameter);
            }
            if (syntaxVisitor.IsNonNullFlow(operation.Syntax)) {
                parameterType = parameterType.WithNode(typeSystem.NonNullNode);
            }
            return parameterType;
        }

        public override TypeWithNode VisitInstanceReference(IInstanceReferenceOperation operation, EdgeBuildingContext argument)
        {
            switch (operation.ReferenceKind) {
                case InstanceReferenceKind.ContainingTypeInstance:
                    return new TypeWithNode(operation.Type, typeSystem.NonNullNode);
                case InstanceReferenceKind.ImplicitReceiver:
                    return currentObjectCreationType;
                default:
                    throw new NotImplementedException(operation.ReferenceKind.ToString());
            }
        }

        private void Dereference(TypeWithNode? type, IOperation dereferencingOperation)
        {
            if (type != null) {
                var edge = tsBuilder.CreateEdge(type.Value.Node, typeSystem.NonNullNode);
                edge?.SetLabel("Deref", dereferencingOperation.Syntax.GetLocation());
            }
        }

        public override TypeWithNode VisitFieldReference(IFieldReferenceOperation operation, EdgeBuildingContext argument)
        {
            var receiverType = operation.Instance?.Accept(this, argument);
            Dereference(receiverType, operation);
            if (receiverType == null && operation.Syntax is MemberAccessExpressionSyntax { Expression: var receiverSyntax }) {
                // Look for a syntactic type as in "SomeClass<T>.StaticField"
                receiverType = receiverSyntax.Accept(syntaxVisitor);
            }
            var substitution = new TypeSubstitution(receiverType?.TypeArguments ?? new TypeWithNode[0], new TypeWithNode[0]);
            var fieldType = typeSystem.GetSymbolType(operation.Field.OriginalDefinition);
            fieldType = fieldType.WithSubstitution(operation.Field.Type, substitution);
            if (syntaxVisitor.IsNonNullFlow(operation.Syntax)) {
                fieldType = fieldType.WithNode(typeSystem.NonNullNode);
            }
            return fieldType;
        }

        public override TypeWithNode VisitPropertyReference(IPropertyReferenceOperation operation, EdgeBuildingContext argument)
        {
            var receiverType = operation.Instance?.Accept(this, argument);
            Dereference(receiverType, operation);
            if (receiverType == null && operation.Syntax is MemberAccessExpressionSyntax { Expression: var receiverSyntax }) {
                // Look for a syntactic type as in "SomeClass<T>.StaticProperty"
                receiverType = receiverSyntax.Accept(syntaxVisitor);
            }
            var substitution = new TypeSubstitution(receiverType?.TypeArguments ?? new TypeWithNode[0], new TypeWithNode[0]);
            HandleArguments(substitution, operation.Arguments, context: argument);
            var propertyType = typeSystem.GetSymbolType(operation.Property.OriginalDefinition);
            propertyType = propertyType.WithSubstitution(operation.Property.Type, substitution);
            if (syntaxVisitor.IsNonNullFlow(operation.Syntax)) {
                propertyType = propertyType.WithNode(typeSystem.NonNullNode);
            }
            return propertyType;
        }

        public override TypeWithNode VisitInvocation(IInvocationOperation operation, EdgeBuildingContext argument)
        {
            var receiverType = operation.Instance?.Accept(this, argument);
            Dereference(receiverType, operation);
            if (receiverType == null && operation.Syntax is InvocationExpressionSyntax { Expression: MemberAccessExpressionSyntax { Expression: var receiverSyntax } }) {
                // Look for a syntactic type as in "SomeClass<T>.StaticMethod();"
                receiverType = receiverSyntax.Accept(syntaxVisitor);
            }
            var classTypeArgNodes = receiverType?.TypeArguments ?? new TypeWithNode[0];
            TypeWithNode[]? methodTypeArgNodes = null;
            if (operation.Syntax is InvocationExpressionSyntax ies) {
                var typeArgSyntax = FindTypeArgumentList(ies.Expression);
                methodTypeArgNodes = typeArgSyntax?.Arguments.Select(syntaxVisitor.Visit).ToArray();
            }
            // If there are no syntactic type arguments, create temporary type nodes instead to represent
            // the inferred type arguments.
            methodTypeArgNodes ??= operation.TargetMethod.TypeArguments.Select(tsBuilder.CreateTemporaryType).ToArray();
            var substitution = new TypeSubstitution(classTypeArgNodes, methodTypeArgNodes);
            HandleArguments(substitution, operation.Arguments, context: argument);
            var returnType = typeSystem.GetSymbolType(operation.TargetMethod.OriginalDefinition);
            returnType = returnType.WithSubstitution(operation.TargetMethod.ReturnType, substitution);
            return returnType;
        }

        private TypeArgumentListSyntax? FindTypeArgumentList(ExpressionSyntax expr) => expr switch
        {
            GenericNameSyntax gns => gns.TypeArgumentList,
            MemberAccessExpressionSyntax maes => FindTypeArgumentList(maes.Name),
            AliasQualifiedNameSyntax aqns => FindTypeArgumentList(aqns.Name),
            _ => null,
        };

        private TypeSubstitution HandleArguments(TypeSubstitution substitution, ImmutableArray<IArgumentOperation> arguments, EdgeBuildingContext context)
        {
            foreach (var arg in arguments) {
                var param = arg.Parameter.OriginalDefinition;
                var parameterType = typeSystem.GetSymbolType(param);
                var argumentType = arg.Value.Accept(this, context);
                // Create an assignment edge from argument to parameter.
                // We use the parameter's original type + substitution so that a type parameter `T` appearing in
                // multiple parameters uses the same nullability nodes for all occurrences.
                VarianceKind variance = param.RefKind switch
                {
                    // The direction of the edge depends on the refkind:
                    RefKind.None => VarianceKind.Out, // argument --> parameter
                    RefKind.In => VarianceKind.Out,   // argument --> parameter
                    RefKind.Ref => VarianceKind.None, // argument <-> parameter
                    RefKind.Out => VarianceKind.In,   // argument <-- parameter
                    _ => throw new NotSupportedException($"RefKind unsupported: {param.RefKind}")
                };
                var edge = tsBuilder.CreateTypeEdge(source: argumentType, target: parameterType, substitution, variance);
                edge?.SetLabel("Argument", arg.Syntax?.GetLocation());
            }
            return substitution;
        }

        private TypeWithNode currentObjectCreationType;

        public override TypeWithNode VisitObjectCreation(IObjectCreationOperation operation, EdgeBuildingContext argument)
        {
            if (operation.Syntax is ObjectCreationExpressionSyntax syntax) {
                var newObjectType = syntax.Type.Accept(syntaxVisitor);
                var substitution = new TypeSubstitution(currentObjectCreationType.TypeArguments, new TypeWithNode[0]);
                HandleArguments(substitution, operation.Arguments, context: argument);

                var oldObjectCreationType = currentObjectCreationType;
                try {
                    currentObjectCreationType = newObjectType;
                    operation.Initializer?.Accept(this, argument);
                } finally {
                    currentObjectCreationType = oldObjectCreationType;
                }
                return newObjectType;
            } else {
                throw new NotImplementedException($"ObjectCreationOperation with syntax={operation.Syntax}");
            }
        }

        public override TypeWithNode VisitArrayCreation(IArrayCreationOperation operation, EdgeBuildingContext argument)
        {
            foreach (var op in operation.DimensionSizes) {
                op.Accept(this, argument);
            }

            if (operation.Syntax is ArrayCreationExpressionSyntax syntax) {
                var arrayType = syntax.Type.Accept(syntaxVisitor);
                arrayType.SetName("ArrayCreation");
                if (operation.Initializer != null) {
                    HandleArrayInitializer(operation.Initializer, arrayType, argument);
                }
                return arrayType;
            } else if (operation.Syntax is ImplicitArrayCreationExpressionSyntax) {
                var arrayType = tsBuilder.CreateTemporaryType(operation.Type);
                arrayType.SetName("ArrayCreation");
                if (operation.Initializer != null) {
                    HandleArrayInitializer(operation.Initializer, arrayType, argument);
                }
                return arrayType;
            } else {
                throw new NotImplementedException($"ArrayCreationOperation with syntax={operation.Syntax}");
            }
        }

        public override TypeWithNode VisitArrayElementReference(IArrayElementReferenceOperation operation, EdgeBuildingContext argument)
        {
            var arrayType = operation.ArrayReference.Accept(this, argument);
            Dereference(arrayType, operation);
            foreach (var index in operation.Indices) {
                index.Accept(this, argument);
            }
            return arrayType.TypeArguments.Single();
        }

        private void HandleArrayInitializer(IArrayInitializerOperation operation, TypeWithNode arrayType, EdgeBuildingContext argument)
        {
            TypeWithNode elementType = arrayType.TypeArguments.Single();
            foreach (var elementInit in operation.ElementValues) {
                var initType = elementInit.Accept(this, argument);
                var edge = tsBuilder.CreateAssignmentEdge(source: initType, target: elementType);
                edge?.SetLabel("ArrayInit", elementInit.Syntax?.GetLocation());
            }
        }

        public override TypeWithNode VisitPropertyInitializer(IPropertyInitializerOperation operation, EdgeBuildingContext argument)
        {
            var property = operation.InitializedProperties.Single();
            var propertyType = typeSystem.GetSymbolType(property);
            var value = operation.Value.Accept(this, argument);
            var edge = tsBuilder.CreateAssignmentEdge(source: value, target: propertyType);
            edge?.SetLabel("PropertyInit", operation.Syntax?.GetLocation());
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitFieldInitializer(IFieldInitializerOperation operation, EdgeBuildingContext argument)
        {
            var field = operation.InitializedFields.Single();
            var fieldType = typeSystem.GetSymbolType(field);
            var value = operation.Value.Accept(this, argument);
            var edge = tsBuilder.CreateAssignmentEdge(source: value, target: fieldType);
            edge?.SetLabel("FieldInit", operation.Syntax?.GetLocation());
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitLiteral(ILiteralOperation operation, EdgeBuildingContext argument)
        {
            if (operation.Type?.IsValueType == true) {
                return new TypeWithNode(operation.Type, typeSystem.ObliviousNode);
            } else if (operation.ConstantValue.HasValue && operation.ConstantValue.Value == null) {
                return new TypeWithNode(operation.Type, typeSystem.NullableNode);
            } else {
                return new TypeWithNode(operation.Type, typeSystem.NonNullNode);
            }
        }

        public override TypeWithNode VisitDefaultValue(IDefaultValueOperation operation, EdgeBuildingContext argument)
        {
            var type = typeSystem.GetObliviousType(operation.Type);
            if (operation.Type.IsReferenceType) {
                type = type.WithNode(typeSystem.NullableNode);
            }
            return type;
        }

        public override TypeWithNode VisitNameOf(INameOfOperation operation, EdgeBuildingContext argument)
        {
            return new TypeWithNode(operation.Type, typeSystem.NonNullNode);
        }

        public override TypeWithNode VisitConversion(IConversionOperation operation, EdgeBuildingContext argument)
        {
            if (operation.OperatorMethod != null)
                throw new NotImplementedException("Overloaded conversion operator");
            var input = operation.Operand.Accept(this, argument);
            var conv = operation.GetConversion();
            if (conv.IsThrow || conv.IsConstantExpression) {
                return typeSystem.GetObliviousType(operation.Type);
            } else if (conv.IsReference || conv.IsIdentity) {
                TypeWithNode targetType;
                if (operation.Syntax is CastExpressionSyntax cast) {
                    targetType = cast.Type.Accept(syntaxVisitor);
                } else {
                    targetType = tsBuilder.CreateTemporaryType(operation.Type);
                    targetType.SetName(conv.ToString() + "Conversion");
                }
                // TODO: handle type arguments
                var edge = tsBuilder.CreateEdge(source: input.Node, target: targetType.Node);
                edge?.SetLabel("Cast", operation.Syntax?.GetLocation());
                return targetType;
            } else if (conv.IsDefaultLiteral) {
                Debug.Assert(SymbolEqualityComparer.Default.Equals(input.Type, operation.Type));
                return input;
            } else {
                throw new NotImplementedException($"Unknown conversion: {conv}");
            }
        }

        private TypeWithNode conditionalAccessInstance;

        public override TypeWithNode VisitConditionalAccess(IConditionalAccessOperation operation, EdgeBuildingContext argument)
        {
            var oldConditionalAccessInstance = conditionalAccessInstance;
            try {
                var target = operation.Operation.Accept(this, argument);
                conditionalAccessInstance = target.WithNode(typeSystem.NonNullNode);
                var value = operation.WhenNotNull.Accept(this, argument);
                return value.WithNode(typeSystem.NullableNode);
            } finally {
                conditionalAccessInstance = oldConditionalAccessInstance;
            }
        }

        public override TypeWithNode VisitConditionalAccessInstance(IConditionalAccessInstanceOperation operation, EdgeBuildingContext argument)
        {
            return conditionalAccessInstance;
        }

        public override TypeWithNode VisitSimpleAssignment(ISimpleAssignmentOperation operation, EdgeBuildingContext argument)
        {
            var target = operation.Target.Accept(this, argument);
            var value = operation.Value.Accept(this, argument);
            var edge = tsBuilder.CreateAssignmentEdge(source: value, target: target);
            edge?.SetLabel("Assign", operation.Syntax?.GetLocation());
            return target;
        }

        public override TypeWithNode VisitVariableDeclarationGroup(IVariableDeclarationGroupOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Children) {
                child.Accept(this, argument);
            }
            return typeSystem.VoidType;
        }

        // Maps implicitly-typed local variables (or lambda parameters) to their inferred type.
        private readonly Dictionary<ISymbol, TypeWithNode> localVarTypes = new Dictionary<ISymbol, TypeWithNode>();
        private readonly List<ISymbol> localVariables = new List<ISymbol>(); // used to remove dictionary entries at end of block

        public override TypeWithNode VisitVariableDeclaration(IVariableDeclarationOperation operation, EdgeBuildingContext argument)
        {
            if (operation.Syntax is VariableDeclarationSyntax { Type: SimpleNameSyntax { IsVar: true } }) {
                // Implicitly typed local variable.
                // We syntactically can't use "var?", an implicitly typed variable is forced to use
                // the same nullability as inferred from its initializer expression.
                foreach (var decl in operation.Declarators) {
                    var init = decl.Initializer.Accept(this, argument);
                    localVarTypes.Add(decl.Symbol, init);
                    localVariables.Add(decl.Symbol);
                }
            } else {
                foreach (var child in operation.Children) {
                    child.Accept(this, argument);
                }
            }
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitVariableDeclarator(IVariableDeclaratorOperation operation, EdgeBuildingContext argument)
        {
            var variableType = typeSystem.GetSymbolType(operation.Symbol);
            if (operation.Initializer != null) {
                var init = operation.Initializer.Accept(this, argument);
                var edge = tsBuilder.CreateAssignmentEdge(source: init, target: variableType);
                edge?.SetLabel("VarInit", operation.Syntax?.GetLocation());
            }
            return variableType;
        }

        public override TypeWithNode VisitVariableInitializer(IVariableInitializerOperation operation, EdgeBuildingContext argument)
        {
            return operation.Value.Accept(this, argument);
        }

        public override TypeWithNode VisitObjectOrCollectionInitializer(IObjectOrCollectionInitializerOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Children) {
                child.Accept(this, argument);
            }
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitTuple(ITupleOperation operation, EdgeBuildingContext argument)
        {
            var elementTypes = operation.Elements.Select(e => e.Accept(this, argument)).ToArray();
            return new TypeWithNode(operation.Type, typeSystem.ObliviousNode, elementTypes);
        }

        public override TypeWithNode VisitDeconstructionAssignment(IDeconstructionAssignmentOperation operation, EdgeBuildingContext argument)
        {
            var lhs = operation.Target.Accept(this, argument);
            var rhs = operation.Value.Accept(this, argument);
            if (lhs.Type?.IsTupleType == true) {
                Debug.Assert(lhs.TypeArguments.Count == rhs.TypeArguments.Count);
                foreach (var (lhsElement, rhsElement) in lhs.TypeArguments.Zip(rhs.TypeArguments)) {
                    tsBuilder.CreateAssignmentEdge(rhsElement, lhsElement);
                }
                return rhs;
            } else {
                throw new NotImplementedException("DeconstructionAssignment for non-tuple");
            }
        }

        public override TypeWithNode VisitDeclarationExpression(IDeclarationExpressionOperation operation, EdgeBuildingContext argument)
        {
            // appears e.g. in `var (a, b) = tuple;`
            return operation.Expression.Accept(this, argument);
        }

        public override TypeWithNode VisitDiscardOperation(IDiscardOperation operation, EdgeBuildingContext argument)
        {
            // appears e.g. in `_ = string.Empty;`
            return typeSystem.GetObliviousType(operation.Type);
        }

        public override TypeWithNode VisitDelegateCreation(IDelegateCreationOperation operation, EdgeBuildingContext argument)
        {
            var delegateType = operation.Type as INamedTypeSymbol;
            if (delegateType?.DelegateInvokeMethod == null)
                throw new NotSupportedException("Could not find Invoke() method for delegate");
            var type = new TypeWithNode(operation.Type, typeSystem.NonNullNode, delegateType.TypeArguments.Select(tsBuilder.CreateTemporaryType).ToArray());
            type.SetName("delegate");
            var substitution = new TypeSubstitution(type.TypeArguments, new TypeWithNode[0]);
            var delegateReturnType = typeSystem.GetSymbolType(delegateType.DelegateInvokeMethod.OriginalDefinition);
            delegateReturnType = delegateReturnType.WithSubstitution(delegateType.DelegateInvokeMethod.ReturnType, substitution);
            var delegateParameters = delegateType.DelegateInvokeMethod.Parameters
                .Select(p => typeSystem.GetSymbolType(p.OriginalDefinition).WithSubstitution(p.Type, substitution)).ToArray();
            switch (operation.Target) {
                case IAnonymousFunctionOperation lambda:
                    // Create edges for lambda parameters
                    var parameterList = lambda.Syntax switch
                    {
                        SimpleLambdaExpressionSyntax syntax => (IReadOnlyList<ParameterSyntax>)new[] { syntax.Parameter },
                        ParenthesizedLambdaExpressionSyntax lambdaSyntax => lambdaSyntax.ParameterList.Parameters,
                        _ => throw new NotImplementedException($"Unsupported syntax for lambdas: {lambda.Syntax}")
                    };
                    Debug.Assert(parameterList.Count == delegateParameters.Length);
                    foreach (var (lambdaParamSyntax, invokeParam) in parameterList.Zip(delegateParameters)) {
                        if (lambdaParamSyntax.Type != null) {
                            var paramType = lambdaParamSyntax.Type.Accept(syntaxVisitor);
                            tsBuilder.CreateAssignmentEdge(invokeParam, paramType)?.SetLabel("lambda parameter", lambdaParamSyntax.GetLocation());
                        } else {
                            // Implicitly typed lambda parameter: treat like a `var` variable initialization
                            var lambdaParamSymbol = syntaxVisitor.semanticModel.GetDeclaredSymbol(lambdaParamSyntax);
                            if (lambdaParamSymbol == null)
                                throw new InvalidOperationException("Could not find symbol for lambda parameter");
                            localVarTypes.Add(lambdaParamSymbol, invokeParam);
                        }
                    }
                    // Analyze the body, and treat any `return` statements as assignments to `delegateReturnType`.
                    var outerMethodReturnType = syntaxVisitor.currentMethodReturnType;
                    try {
                        syntaxVisitor.currentMethodReturnType = delegateReturnType;
                        lambda.Body.Accept(this, argument);
                    } finally {
                        syntaxVisitor.currentMethodReturnType = outerMethodReturnType;
                    }
                    break;
                default:
                    throw new NotImplementedException($"DelegateCreation with {operation.Target}");
            }
            return type;
        }
    }
}
