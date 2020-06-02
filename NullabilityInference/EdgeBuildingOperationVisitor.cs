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
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Operations;

namespace ICSharpCode.NullabilityInference
{
    internal enum EdgeBuildingContext
    {
        /// <summary>
        /// Normal mode.
        /// </summary>
        Normal,
        /// <summary>
        /// Translation as LValue for assignments.
        /// In this mode, the flow state is ignored.
        /// </summary>
        LValue
    }

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
                child.Accept(this, EdgeBuildingContext.Normal);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitConstructorBodyOperation(IConstructorBodyOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Children)
                child.Accept(this, EdgeBuildingContext.Normal);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitBlock(IBlockOperation operation, EdgeBuildingContext argument)
        {
            int oldVariableCount = localVariables.Count;
            foreach (var child in operation.Operations)
                child.Accept(this, EdgeBuildingContext.Normal);
            // clean up all variables added within the block
            while (localVariables.Count > oldVariableCount) {
                localVarTypes.Remove(localVariables[localVariables.Count - 1]);
                localVariables.RemoveAt(localVariables.Count - 1);
            }
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitSwitch(ISwitchOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Children)
                child.Accept(this, EdgeBuildingContext.Normal);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitSwitchCase(ISwitchCaseOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Body)
                child.Accept(this, EdgeBuildingContext.Normal);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitTry(ITryOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Children)
                child.Accept(this, EdgeBuildingContext.Normal);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitCatchClause(ICatchClauseOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Children)
                child.Accept(this, EdgeBuildingContext.Normal);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitForLoop(IForLoopOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Children)
                child.Accept(this, EdgeBuildingContext.Normal);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitWhileLoop(IWhileLoopOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Children)
                child.Accept(this, EdgeBuildingContext.Normal);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitForEachLoop(IForEachLoopOperation operation, EdgeBuildingContext argument)
        {
            var loopInfo = syntaxVisitor.semanticModel.GetForEachStatementInfo((CommonForEachStatementSyntax)operation.Syntax);
            TypeWithNode collection;
            TypeWithNode elementType;
            if (operation.Collection is IConversionOperation { IsImplicit: true, Conversion: { IsReference: true }, Operand: { Type: { TypeKind: TypeKind.Array } } arrayOperand }) {
                // special case: the operation tree pretends that non-generic IEnumerable is used when iterating over arrays,
                // but that would cause us to lose information about the element's nullability.
                collection = arrayOperand.Accept(this, EdgeBuildingContext.Normal);
                elementType = collection.TypeArguments.Single();
            } else {
                collection = operation.Collection.Accept(this, EdgeBuildingContext.Normal);

                // Determine the enumerator type (which might have nullabilities dependent on type arguments from the collection type)
                if (loopInfo.GetEnumeratorMethod == null)
                    throw new NotSupportedException("foreach loop without GetEnumeratorMethod");
                var getEnumeratorSubstitution = SubstitutionForMemberAccess(collection, loopInfo.GetEnumeratorMethod);
                var enumeratorType = typeSystem.GetSymbolType(loopInfo.GetEnumeratorMethod.OriginalDefinition);
                enumeratorType = enumeratorType.WithSubstitution(loopInfo.GetEnumeratorMethod.ReturnType, getEnumeratorSubstitution);

                // Determine the element type (which might have nullabilities dependent on type arguments from the enumerator type)
                if (loopInfo.CurrentProperty == null)
                    throw new NotSupportedException("foreach loop without CurrentProperty");
                var getCurrentSubstitution = SubstitutionForMemberAccess(enumeratorType, loopInfo.CurrentProperty);
                elementType = typeSystem.GetSymbolType(loopInfo.CurrentProperty.OriginalDefinition);
                elementType = elementType.WithSubstitution(loopInfo.CurrentProperty.Type, getCurrentSubstitution);
            }
            Dereference(collection, operation);
            var loopVariable = operation.LoopControlVariable.Accept(this, EdgeBuildingContext.LValue);

            CreateConversionEdge(elementType, loopVariable, loopInfo.ElementConversion, new EdgeLabel("loop variable", operation));
            operation.Body.Accept(this, EdgeBuildingContext.Normal);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitBranch(IBranchOperation operation, EdgeBuildingContext argument)
        {
            // goto / break / continue
            foreach (var child in operation.Children)
                child.Accept(this, EdgeBuildingContext.Normal);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitUsing(IUsingOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Children)
                child.Accept(this, EdgeBuildingContext.Normal);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitLock(ILockOperation operation, EdgeBuildingContext argument)
        {
            var monitor = operation.LockedValue.Accept(this, EdgeBuildingContext.Normal);
            Dereference(monitor, operation);
            operation.Body.Accept(this, EdgeBuildingContext.Normal);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitEmpty(IEmptyOperation operation, EdgeBuildingContext argument)
        {
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitExpressionStatement(IExpressionStatementOperation operation, EdgeBuildingContext argument)
        {
            operation.Operation.Accept(this, EdgeBuildingContext.Normal);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitReturn(IReturnOperation operation, EdgeBuildingContext argument)
        {
            if (operation.ReturnedValue != null) {
                var returnVal = operation.ReturnedValue.Accept(this, EdgeBuildingContext.Normal);
                var returnType = syntaxVisitor.currentMethodReturnType;
                if (operation.Kind == OperationKind.YieldReturn) {
                    if (returnType.TypeArguments.Count == 0) {
                        // returning non-generic enumerable
                        return typeSystem.VoidType;
                    }
                    returnType = returnType.TypeArguments.Single();
                }
                tsBuilder.CreateAssignmentEdge(
                    source: returnVal,
                    target: returnType,
                    label: new EdgeLabel("return", operation));
            }
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitConditional(IConditionalOperation operation, EdgeBuildingContext argument)
        {
            operation.Condition.Accept(this, EdgeBuildingContext.Normal);

            var mergedType = tsBuilder.CreateTemporaryType(operation.Type);
            mergedType.SetName("?:");

            var whenTrue = operation.WhenTrue.Accept(this, argument);
            tsBuilder.CreateAssignmentEdge(whenTrue, mergedType, new EdgeLabel("then", operation.WhenTrue));

            if (operation.WhenFalse != null) {
                var whenFalse = operation.WhenFalse.Accept(this, argument);
                tsBuilder.CreateAssignmentEdge(whenFalse, mergedType, new EdgeLabel("else", operation.WhenFalse));
            }

            return mergedType;
        }

        public override TypeWithNode VisitUnaryOperator(IUnaryOperation operation, EdgeBuildingContext argument)
        {
            var operand = operation.Operand.Accept(this, EdgeBuildingContext.Normal);
            if (operation.OperatorMethod != null) {
                var operatorParams = operation.OperatorMethod.Parameters;
                var param = typeSystem.GetSymbolType(operatorParams.Single());
                if (operation.IsLifted) {
                    if (operand.Type.IsSystemNullable() && !param.Type.IsSystemNullable()) {
                        operand = operand.TypeArguments.Single();
                    }
                }
                tsBuilder.CreateAssignmentEdge(operand, param, new EdgeLabel("unary operand", operation));
                var operatorReturnType = typeSystem.GetSymbolType(operation.OperatorMethod);
                if (operation.IsLifted) {
                    if (operatorReturnType.Type?.IsReferenceType == true) {
                        operatorReturnType = operatorReturnType.WithNode(typeSystem.NullableNode);
                    } else if (operation.Type.IsSystemNullable() && !operatorReturnType.Type.IsSystemNullable()) {
                        // wrap in Nullable<T>
                        operatorReturnType = new TypeWithNode(operation.Type, typeSystem.ObliviousNode, new[] { operatorReturnType });
                    }
                }

                return operatorReturnType;
            }
            return typeSystem.GetObliviousType(operation.Type);
        }

        public override TypeWithNode VisitIncrementOrDecrement(IIncrementOrDecrementOperation operation, EdgeBuildingContext argument)
        {
            if (operation.OperatorMethod != null)
                throw new NotImplementedException("Overloaded operator");
            operation.Target.Accept(this, EdgeBuildingContext.Normal);
            return typeSystem.GetObliviousType(operation.Type);
        }

        public override TypeWithNode VisitBinaryOperator(IBinaryOperation operation, EdgeBuildingContext argument)
        {
            var lhs = operation.LeftOperand.Accept(this, EdgeBuildingContext.Normal);
            var rhs = operation.RightOperand.Accept(this, EdgeBuildingContext.Normal);
            if (operation.OperatorMethod != null) {
                return HandleOverloadedBinaryOperator(operation, lhs, rhs, operation.OperatorMethod, operation.IsLifted);
            }
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

        private TypeWithNode HandleOverloadedBinaryOperator(IOperation operation, TypeWithNode lhs, TypeWithNode rhs, IMethodSymbol operatorMethod, bool isLifted)
        {
            var operatorParams = operatorMethod.Parameters;
            Debug.Assert(operatorParams.Length == 2);
            var lhsParam = typeSystem.GetSymbolType(operatorParams[0]);
            var rhsParam = typeSystem.GetSymbolType(operatorParams[1]);
            if (isLifted) {
                if (lhs.Type.IsSystemNullable() && !lhsParam.Type.IsSystemNullable()) {
                    lhs = lhs.TypeArguments.Single();
                }
                if (rhs.Type.IsSystemNullable() && !rhsParam.Type.IsSystemNullable()) {
                    rhs = rhs.TypeArguments.Single();
                }
            }
            tsBuilder.CreateAssignmentEdge(lhs, lhsParam, new EdgeLabel("binary lhs", operation));
            tsBuilder.CreateAssignmentEdge(rhs, rhsParam, new EdgeLabel("binary rhs", operation));
            var operatorReturnType = typeSystem.GetSymbolType(operatorMethod);
            if (isLifted) {
                if (operatorReturnType.Type?.IsReferenceType == true) {
                    operatorReturnType = operatorReturnType.WithNode(typeSystem.NullableNode);
                } else if (operation.Type.IsSystemNullable() && !operatorReturnType.Type.IsSystemNullable()) {
                    // wrap in Nullable<T>
                    operatorReturnType = new TypeWithNode(operation.Type, typeSystem.ObliviousNode, new[] { operatorReturnType });
                }
            }

            return operatorReturnType;
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
                    if (attr.ConstructorArguments.Length == 1 && attr.AttributeClass?.GetFullName() == "System.Diagnostics.CodeAnalysis.DoesNotReturnIfAttribute") {
                        if (attr.ConstructorArguments.Single().Value is bool val && val == valueOnNull) {
                            tsBuilder.CreateEdge(testedNode.Node, typeSystem.NonNullNode, new EdgeLabel("DoesNotReturnIf", operation));
                            break;
                        }
                    }
                }
            }
        }

        public override TypeWithNode VisitCompoundAssignment(ICompoundAssignmentOperation operation, EdgeBuildingContext argument)
        {
            var lhs = operation.Target.Accept(this, EdgeBuildingContext.LValue);
            var rhs = operation.Value.Accept(this, EdgeBuildingContext.Normal);
            if (operation.OperatorMethod != null) {
                var operatorResult = HandleOverloadedBinaryOperator(operation, lhs, rhs, operation.OperatorMethod, operation.IsLifted);
                CreateConversionEdge(operatorResult, lhs, operation.GetOutConversion(), new EdgeLabel("compound assign", operation));
                return lhs;
            }
            if (operation.Type.TypeKind == TypeKind.Delegate && operation.OperatorKind == BinaryOperatorKind.Add) {
                tsBuilder.CreateAssignmentEdge(rhs.WithNode(typeSystem.ObliviousNode), lhs, new EdgeLabel("delegate combine", operation));
                return lhs;
            } else {
                return typeSystem.GetObliviousType(operation.Type);
            }
        }

        public override TypeWithNode VisitCoalesce(ICoalesceOperation operation, EdgeBuildingContext argument)
        {
            var lhs = operation.Value.Accept(this, EdgeBuildingContext.Normal);
            var rhs = operation.WhenNull.Accept(this, EdgeBuildingContext.Normal);
            var result = tsBuilder.CreateTemporaryType(operation.Type);
            result.SetName("??");
            CreateCastEdge(lhs, result, new EdgeLabel("lhs of ??", operation));
            tsBuilder.CreateAssignmentEdge(rhs, result, new EdgeLabel("rhs of ??", operation));
            // for the top-level nullability, only the rhs is relevant
            return result.WithNode(rhs.Node);
        }

        public override TypeWithNode VisitThrow(IThrowOperation operation, EdgeBuildingContext argument)
        {
            var exception = operation.Exception?.Accept(this, EdgeBuildingContext.Normal);
            Dereference(exception, operation);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitLocalReference(ILocalReferenceOperation operation, EdgeBuildingContext argument)
        {
            if (!localVarTypes.TryGetValue(operation.Local, out TypeWithNode variableType)) {
                variableType = typeSystem.GetSymbolType(operation.Local);
            }
            if (argument != EdgeBuildingContext.LValue && syntaxVisitor.IsNonNullFlow(operation.Syntax)) {
                variableType = variableType.WithNode(typeSystem.NonNullNode);
            }
            return variableType;
        }

        public override TypeWithNode VisitParameterReference(IParameterReferenceOperation operation, EdgeBuildingContext argument)
        {
            if (!localVarTypes.TryGetValue(operation.Parameter, out TypeWithNode parameterType)) {
                parameterType = typeSystem.GetSymbolType(operation.Parameter);
            }
            if (argument != EdgeBuildingContext.LValue && syntaxVisitor.IsNonNullFlow(operation.Syntax)) {
                parameterType = parameterType.WithNode(typeSystem.NonNullNode);
            }
            return parameterType;
        }

        public override TypeWithNode VisitInstanceReference(IInstanceReferenceOperation operation, EdgeBuildingContext argument)
        {
            switch (operation.ReferenceKind) {
                case InstanceReferenceKind.ContainingTypeInstance:
                    return typeSystem.GetObliviousType(operation.Type).WithNode(typeSystem.NonNullNode);
                case InstanceReferenceKind.ImplicitReceiver:
                    return currentObjectCreationType;
                default:
                    throw new NotImplementedException(operation.ReferenceKind.ToString());
            }
        }

        private void Dereference(TypeWithNode? type, IOperation dereferencingOperation)
        {
            if (type != null) {
                tsBuilder.CreateEdge(type.Value.Node, typeSystem.NonNullNode, new EdgeLabel("Deref", dereferencingOperation));
            }
        }

        private IReadOnlyList<TypeWithNode> ClassTypeArgumentsForMemberAccess(TypeWithNode? receiverType, ISymbol member)
        {
            if (receiverType != null) {
                receiverType = typeSystem.GetBaseType(receiverType.Value, member.ContainingType.OriginalDefinition);
            }
            if (receiverType == null) {
                receiverType = typeSystem.GetObliviousType(member.ContainingType);
            }
            return receiverType.Value.TypeArguments;
        }

        internal TypeSubstitution SubstitutionForMemberAccess(TypeWithNode? receiverType, ISymbol member)
        {
            return new TypeSubstitution(ClassTypeArgumentsForMemberAccess(receiverType, member), new TypeWithNode[0]);
        }

        public override TypeWithNode VisitFieldReference(IFieldReferenceOperation operation, EdgeBuildingContext argument)
        {
            var receiverType = operation.Instance?.Accept(this, EdgeBuildingContext.Normal);
            Dereference(receiverType, operation);
            if (receiverType == null && operation.Syntax is MemberAccessExpressionSyntax { Expression: var receiverSyntax }) {
                // Look for a syntactic type as in "SomeClass<T>.StaticField"
                receiverType = receiverSyntax.Accept(syntaxVisitor);
            }
            var substitution = SubstitutionForMemberAccess(receiverType, operation.Field);
            var fieldType = typeSystem.GetSymbolType(operation.Field.OriginalDefinition);
            fieldType = fieldType.WithSubstitution(operation.Field.Type, substitution);
            if (argument != EdgeBuildingContext.LValue && syntaxVisitor.IsNonNullFlow(operation.Syntax)) {
                fieldType = fieldType.WithNode(typeSystem.NonNullNode);
            }
            return fieldType;
        }

        public override TypeWithNode VisitPropertyReference(IPropertyReferenceOperation operation, EdgeBuildingContext argument)
        {
            var receiverType = operation.Instance?.Accept(this, EdgeBuildingContext.Normal);
            Dereference(receiverType, operation);
            if (receiverType == null && operation.Syntax is MemberAccessExpressionSyntax { Expression: var receiverSyntax }) {
                // Look for a syntactic type as in "SomeClass<T>.StaticProperty"
                receiverType = receiverSyntax.Accept(syntaxVisitor);
            }
            var substitution = SubstitutionForMemberAccess(receiverType, operation.Property);
            HandleArguments(substitution, operation.Arguments);
            TypeWithNode propertyType;
            if (operation.Property.ContainingType.IsAnonymousType) {
                // For anonymous types, we act as if the member types are all type arguments.
                // Discover the "type argument" index of our property:
                int propIndex = 0;
                foreach (var prop in operation.Property.ContainingType.GetMembers().OfType<IPropertySymbol>()) {
                    if (SymbolEqualityComparer.Default.Equals(prop, operation.Property)) {
                        break;
                    } else {
                        propIndex++;
                    }
                }
                propertyType = substitution.ClassTypeArguments[propIndex];
            } else {
                propertyType = typeSystem.GetSymbolType(operation.Property.OriginalDefinition);
                propertyType = propertyType.WithSubstitution(operation.Property.Type, substitution);
            }
            if (argument != EdgeBuildingContext.LValue && syntaxVisitor.IsNonNullFlow(operation.Syntax)) {
                propertyType = propertyType.WithNode(typeSystem.NonNullNode);
            }
            return propertyType;
        }

        public override TypeWithNode VisitEventReference(IEventReferenceOperation operation, EdgeBuildingContext argument)
        {
            var receiverType = operation.Instance?.Accept(this, EdgeBuildingContext.Normal);
            Dereference(receiverType, operation);
            if (receiverType == null && operation.Syntax is MemberAccessExpressionSyntax { Expression: var receiverSyntax }) {
                // Look for a syntactic type as in "SomeClass<T>.StaticEvent"
                receiverType = receiverSyntax.Accept(syntaxVisitor);
            }
            var substitution = SubstitutionForMemberAccess(receiverType, operation.Event);
            var eventType = typeSystem.GetSymbolType(operation.Event.OriginalDefinition);
            eventType = eventType.WithSubstitution(operation.Event.Type, substitution);
            if (argument != EdgeBuildingContext.LValue && syntaxVisitor.IsNonNullFlow(operation.Syntax)) {
                eventType = eventType.WithNode(typeSystem.NonNullNode);
            }
            return eventType;
        }

        public override TypeWithNode VisitInvocation(IInvocationOperation operation, EdgeBuildingContext argument)
        {
            var receiverType = operation.Instance?.Accept(this, EdgeBuildingContext.Normal);
            Dereference(receiverType, operation);
            if (receiverType == null && operation.Syntax is InvocationExpressionSyntax { Expression: MemberAccessExpressionSyntax { Expression: var receiverSyntax } } invocation) {
                // Look for a syntactic type as in "SomeClass<T>.StaticMethod();"
                // However this might also be a call to an extension method "someExpr.ExtensionMethod()".
                // In this case we don't want to visit the 'this' argument expression twice (once via receiverSyntax,
                // then again via operation.Arguments[0]).
                if (!syntaxVisitor.IsReducedExtensionMethodCall(invocation)) {
                    receiverType = receiverSyntax.Accept(syntaxVisitor);
                }
            }
            var classTypeArgNodes = ClassTypeArgumentsForMemberAccess(receiverType, operation.TargetMethod);
            TypeWithNode[]? methodTypeArgNodes = null;
            if (operation.Syntax is InvocationExpressionSyntax ies) {
                var typeArgSyntax = FindTypeArgumentList(ies.Expression);
                methodTypeArgNodes = typeArgSyntax?.Arguments.Select(syntaxVisitor.Visit).ToArray();
            }
            // If there are no syntactic type arguments, create temporary type nodes instead to represent
            // the inferred type arguments.
            if (methodTypeArgNodes == null) {
                methodTypeArgNodes = operation.TargetMethod.TypeArguments.Select(tsBuilder.CreateTemporaryType).ToArray();
                for (int i = 0; i < methodTypeArgNodes.Length; i++) {
                    methodTypeArgNodes[i].SetName($"{operation.TargetMethod.Name}!!{i}");
                }
            }
            var substitution = new TypeSubstitution(classTypeArgNodes, methodTypeArgNodes);
            HandleArguments(substitution, operation.Arguments);
            var returnType = typeSystem.GetSymbolType(operation.TargetMethod.OriginalDefinition);
            returnType = returnType.WithSubstitution(operation.TargetMethod.ReturnType, substitution);
            return returnType;
        }

        private void HandleMethodGroup(IMethodReferenceOperation operation, TypeWithNode delegateReturnType, TypeWithNode[] delegateParameters)
        {
            var receiverType = operation.Instance?.Accept(this, EdgeBuildingContext.Normal);
            Dereference(receiverType, operation);
            if (receiverType == null && operation.Syntax is MemberAccessExpressionSyntax { Expression: var receiverSyntax }) {
                // Look for a syntactic type as in "SomeClass<T>.StaticMethod"
                receiverType = receiverSyntax.Accept(syntaxVisitor);
            }
            var classTypeArgNodes = ClassTypeArgumentsForMemberAccess(receiverType, operation.Method);
            TypeWithNode[]? methodTypeArgNodes = null;
            if (operation.Syntax is ExpressionSyntax es) {
                var typeArgSyntax = FindTypeArgumentList(es);
                methodTypeArgNodes = typeArgSyntax?.Arguments.Select(syntaxVisitor.Visit).ToArray();
            }
            // If there are no syntactic type arguments, create temporary type nodes instead to represent
            // the inferred type arguments.
            methodTypeArgNodes ??= operation.Method.TypeArguments.Select(tsBuilder.CreateTemporaryType).ToArray();
            var substitution = new TypeSubstitution(classTypeArgNodes, methodTypeArgNodes);
            EdgeLabel label = new EdgeLabel($"MethodGroup", operation);

            Debug.Assert(operation.Method.Parameters.Length == delegateParameters.Length);
            foreach (var (methodParam, delegateParam) in operation.Method.Parameters.Zip(delegateParameters)) {
                var methodParamType = typeSystem.GetSymbolType(methodParam.OriginalDefinition);
                methodParamType = methodParamType.WithSubstitution(methodParam.Type, substitution);
                switch (methodParam.RefKind.ToVariance()) {
                    case VarianceKind.In:
                        CreateCastEdge(delegateParam, methodParamType,label);
                        break;
                    case VarianceKind.Out:
                        CreateCastEdge(methodParamType, delegateParam, label);
                        break;
                    case VarianceKind.None:
                        tsBuilder.CreateTypeEdge(delegateParam, methodParamType, targetSubstitution: null, variance: VarianceKind.None, label: label);
                        break;
                    default:
                        throw new InvalidOperationException("Invalid variance");
                }
            }

            var returnType = typeSystem.GetSymbolType(operation.Method.OriginalDefinition);
            returnType = returnType.WithSubstitution(operation.Method.ReturnType, substitution);
            CreateCastEdge(returnType, delegateReturnType, label);
        }

        private TypeArgumentListSyntax? FindTypeArgumentList(ExpressionSyntax expr) => expr switch
        {
            GenericNameSyntax gns => gns.TypeArgumentList,
            MemberAccessExpressionSyntax maes => FindTypeArgumentList(maes.Name),
            AliasQualifiedNameSyntax aqns => FindTypeArgumentList(aqns.Name),
            _ => null,
        };

        private TypeSubstitution HandleArguments(TypeSubstitution substitution, ImmutableArray<IArgumentOperation> arguments)
        {
            foreach (var arg in arguments) {
                var param = arg.Parameter.OriginalDefinition;
                var parameterType = typeSystem.GetSymbolType(param);
                var argumentType = arg.Value.Accept(this, EdgeBuildingContext.Normal);
                // Create an assignment edge from argument to parameter.
                // We use the parameter's original type + substitution so that a type parameter `T` appearing in
                // multiple parameters uses the same nullability nodes for all occurrences.
                var variance = (param.RefKind.ToVariance(), VarianceKind.In).Combine();
                tsBuilder.CreateTypeEdge(source: argumentType, target: parameterType, substitution, variance, new EdgeLabel("Argument", arg));
            }
            return substitution;
        }

        private TypeWithNode currentObjectCreationType;

        public override TypeWithNode VisitObjectCreation(IObjectCreationOperation operation, EdgeBuildingContext argument)
        {
            if (operation.Syntax is ObjectCreationExpressionSyntax syntax) {
                var newObjectType = syntax.Type.Accept(syntaxVisitor).WithNode(typeSystem.NonNullNode);
                var substitution = new TypeSubstitution(newObjectType.TypeArguments, new TypeWithNode[0]);
                HandleArguments(substitution, operation.Arguments);

                var oldObjectCreationType = currentObjectCreationType;
                try {
                    currentObjectCreationType = newObjectType;
                    operation.Initializer?.Accept(this, EdgeBuildingContext.Normal);
                } finally {
                    currentObjectCreationType = oldObjectCreationType;
                }
                return newObjectType;
            } else {
                throw new NotImplementedException($"ObjectCreationOperation with syntax={operation.Syntax}");
            }
        }

        public override TypeWithNode VisitTypeParameterObjectCreation(ITypeParameterObjectCreationOperation operation, EdgeBuildingContext argument)
        {
            var newObjectType = new TypeWithNode(operation.Type, typeSystem.NonNullNode);
            var oldObjectCreationType = currentObjectCreationType;
            try {
                currentObjectCreationType = newObjectType;
                operation.Initializer?.Accept(this, EdgeBuildingContext.Normal);
            } finally {
                currentObjectCreationType = oldObjectCreationType;
            }
            return newObjectType;
        }

        public override TypeWithNode VisitAnonymousObjectCreation(IAnonymousObjectCreationOperation operation, EdgeBuildingContext argument)
        {
            var newObjectType = tsBuilder.CreateTemporaryType(operation.Type);
            newObjectType.SetName("AnonymousObject");
            var oldObjectCreationType = currentObjectCreationType;
            try {
                currentObjectCreationType = newObjectType;
                foreach (var init in operation.Initializers)
                    init.Accept(this, EdgeBuildingContext.Normal);
            } finally {
                currentObjectCreationType = oldObjectCreationType;
            }
            return newObjectType;
        }

        public override TypeWithNode VisitArrayCreation(IArrayCreationOperation operation, EdgeBuildingContext argument)
        {
            foreach (var op in operation.DimensionSizes) {
                op.Accept(this, EdgeBuildingContext.Normal);
            }

            TypeWithNode arrayType;
            if (operation.IsImplicit) {
                // e.g. call with params array
                arrayType = tsBuilder.CreateTemporaryType(operation.Type);
            } else if (operation.Syntax is ArrayCreationExpressionSyntax syntax) {
                arrayType = syntax.Type.Accept(syntaxVisitor);
            } else if (operation.Syntax is ImplicitArrayCreationExpressionSyntax) {
                // implicitly-typed 'new[] { ... }'
                arrayType = tsBuilder.CreateTemporaryType(operation.Type);
            } else {
                throw new NotImplementedException($"ArrayCreationOperation with syntax={operation.Syntax}");
            }
            arrayType.SetName("ArrayCreation");
            if (operation.Initializer != null) {
                HandleArrayInitializer(operation.Initializer, arrayType);
            }
            return arrayType;
        }

        public override TypeWithNode VisitArrayElementReference(IArrayElementReferenceOperation operation, EdgeBuildingContext argument)
        {
            var arrayType = operation.ArrayReference.Accept(this, EdgeBuildingContext.Normal);
            Dereference(arrayType, operation);
            foreach (var index in operation.Indices) {
                index.Accept(this, EdgeBuildingContext.Normal);
            }
            return arrayType.TypeArguments.Single();
        }

        private void HandleArrayInitializer(IArrayInitializerOperation operation, TypeWithNode arrayType)
        {
            TypeWithNode elementType = arrayType.TypeArguments.Single();
            foreach (var elementInit in operation.ElementValues) {
                var initType = elementInit.Accept(this, EdgeBuildingContext.Normal);
                tsBuilder.CreateAssignmentEdge(source: initType, target: elementType, new EdgeLabel("ArrayInit", elementInit));
            }
        }

        public override TypeWithNode VisitPropertyInitializer(IPropertyInitializerOperation operation, EdgeBuildingContext argument)
        {
            var property = operation.InitializedProperties.Single();
            var propertyType = typeSystem.GetSymbolType(property);
            var value = operation.Value.Accept(this, EdgeBuildingContext.Normal);
            tsBuilder.CreateAssignmentEdge(source: value, target: propertyType, new EdgeLabel("PropertyInit", operation));
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitFieldInitializer(IFieldInitializerOperation operation, EdgeBuildingContext argument)
        {
            var field = operation.InitializedFields.Single();
            var fieldType = typeSystem.GetSymbolType(field);
            var value = operation.Value.Accept(this, EdgeBuildingContext.Normal);
            if (fieldType.Type is INamedTypeSymbol { TypeKind: TypeKind.Enum, EnumUnderlyingType: var underlyingType }) {
                // Special case: enum member declarations can directly assign the underlying type without a conversion node
                if (SymbolEqualityComparer.Default.Equals(value.Type, underlyingType)) {
                    fieldType = new TypeWithNode(underlyingType, fieldType.Node);
                }
            }
            tsBuilder.CreateAssignmentEdge(source: value, target: fieldType, new EdgeLabel("FieldInit", operation));
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

        public override TypeWithNode VisitTypeOf(ITypeOfOperation operation, EdgeBuildingContext argument)
        {
            return new TypeWithNode(operation.Type, typeSystem.NonNullNode);
        }

        public override TypeWithNode VisitIsType(IIsTypeOperation operation, EdgeBuildingContext argument)
        {
            operation.ValueOperand.Accept(this, EdgeBuildingContext.Normal);
            return new TypeWithNode(operation.Type, typeSystem.ObliviousNode);
        }

        public override TypeWithNode VisitConversion(IConversionOperation operation, EdgeBuildingContext argument)
        {
            var input = operation.Operand.Accept(this, EdgeBuildingContext.Normal);
            var conv = operation.GetConversion();
            SyntaxNode? syntax = operation.IsImplicit ? null : operation.Syntax;
            TypeWithNode targetType;
            if (syntax is CastExpressionSyntax cast) {
                targetType = cast.Type.Accept(syntaxVisitor);
            } else if (syntax is BinaryExpressionSyntax binary && binary.IsKind(SyntaxKind.AsExpression)) {
                Debug.Assert(operation.IsTryCast);
                targetType = binary.Right.Accept(syntaxVisitor).WithNode(typeSystem.NullableNode);
            } else {
                Debug.Assert(!operation.IsTryCast);
                if (operation.Type.FullArity() == 0 && operation.OperatorMethod == null) {
                    // Optimization: avoid constructing a temporary type node
                    // for simple casts that don't involve generics.
                    return new TypeWithNode(operation.Type, input.Node);
                }
                targetType = tsBuilder.CreateTemporaryType(operation.Type);
                targetType.SetName($"{conv}Conversion");
            }
            CreateConversionEdge(input, targetType, conv, new EdgeLabel($"{conv}Conversion", operation));
            return targetType;
        }

        private void CreateConversionEdge(TypeWithNode input, TypeWithNode target, Conversion conv, EdgeLabel label)
        {
            if (conv.IsUserDefined) {
                var param = conv.MethodSymbol!.Parameters.Single();
                // TODO: handle operator methods within generic types
                var paramType = typeSystem.GetSymbolType(param);
                CreateCastEdge(input, paramType, label);

                // use return type of user-defined operator as input for the remaining conversion
                var returnType = typeSystem.GetSymbolType(conv.MethodSymbol);
                CreateCastEdge(returnType, target, label);
            } else if (conv.IsReference || conv.IsIdentity || conv.IsBoxing || conv.IsUnboxing) {
                CreateCastEdge(input, target, label);
            } else if (conv.IsDefaultLiteral) {
                Debug.Assert(SymbolEqualityComparer.Default.Equals(input.Type, target.Type));
                tsBuilder.CreateTypeEdge(input, target, null, VarianceKind.None, label);
            } else if (conv.IsTupleConversion || conv.IsTupleLiteralConversion) {
                Debug.Assert(input.TypeArguments.Count == target.TypeArguments.Count);
                foreach (var (inputElement, targetElement) in input.TypeArguments.Zip(target.TypeArguments)) {
                    var elementConv = typeSystem.Compilation.ClassifyConversion(inputElement.Type!, targetElement.Type!);
                    CreateConversionEdge(inputElement, targetElement, elementConv, label);
                }
            } else if (conv.IsNullable) {
                if (input.Type.IsSystemNullable()) {
                    input = input.TypeArguments.Single();
                }
                if (target.Type.IsSystemNullable()) {
                    target = target.TypeArguments.Single();
                }
                var elementConv = typeSystem.Compilation.ClassifyConversion(input.Type!, target.Type!);
                if (elementConv.IsNullable)
                    throw new InvalidOperationException("Nullable unwrap failed");
                CreateConversionEdge(input, target, elementConv, label);
            } else if (conv.IsNumeric || conv.IsConstantExpression || conv.IsEnumeration) {
                // OK, no edge required
                Debug.Assert(target.Node.NullType == NullType.Oblivious);
            } else if (conv.IsThrow) {
                // OK, leave target node free-floating
            } else if (conv.IsNullLiteral) {
                tsBuilder.CreateEdge(typeSystem.NullableNode, target.Node, label);
            } else if (!conv.Exists) {
                // Non-existant conversions can occur with "as" syntax; e.g. "IEnumerable<T> as string" compiles
                // and ends up here despite not having a valid conversion.
                // Don't create an edge.
            } else {
                throw new NotImplementedException($"Unknown conversion: {conv}");
            }
        }

        private void CreateCastEdge(TypeWithNode input, TypeWithNode target, EdgeLabel label)
        {
            tsBuilder.CreateEdge(source: input.Node, target: target.Node, label: label);

            if (input.TypeArguments.Count == 0 && target.TypeArguments.Count == 0) {
                // If neither type has additional type arguments, we're done here.
                return;
            }

            if (target.Type is INamedTypeSymbol namedTargetType
                && typeSystem.GetBaseType(input, namedTargetType.OriginalDefinition) is TypeWithNode inputBase) {
                // We might be dealing with
                //     input.Type = Dictionary<string#1, string#2>#3
                // and targetType = IEnumerable<KeyValuePair<string#4, string#5>>#6
                // Then we needs to create edges matching up the key/value type arguments: #4->#1 + #5->#2
                int arity = namedTargetType.FullArity();
                Debug.Assert(inputBase.TypeArguments.Count == arity);
                Debug.Assert(target.TypeArguments.Count == arity);
                var namedTargetTypeTypeParameters = namedTargetType.FullTypeParameters().ToList();
                for (int i = 0; i < arity; i++) {
                    switch (namedTargetTypeTypeParameters[i].Variance) {
                        case VarianceKind.None:
                            tsBuilder.CreateTypeEdge(inputBase.TypeArguments[i], target.TypeArguments[i], targetSubstitution: null, variance: VarianceKind.None, label: label);
                            break;
                        case VarianceKind.Out:
                            CreateCastEdge(inputBase.TypeArguments[i], target.TypeArguments[i], label);
                            break;
                        case VarianceKind.In:
                            CreateCastEdge(target.TypeArguments[i], inputBase.TypeArguments[i], label);
                            break;
                    }
                }
            } else if (input.Type is INamedTypeSymbol namedInputType
                 && typeSystem.GetBaseType(target, namedInputType.OriginalDefinition) is TypeWithNode targetBase) {
                // Same as above, but for casts in the other direction:
                int arity = namedInputType.FullArity();
                Debug.Assert(input.TypeArguments.Count == arity);
                Debug.Assert(targetBase.TypeArguments.Count == arity);
                var namedInputTypeTypeParameters = namedInputType.FullTypeParameters().ToList();
                for (int i = 0; i < arity; i++) {
                    switch (namedInputTypeTypeParameters[i].Variance) {
                        case VarianceKind.None:
                            tsBuilder.CreateTypeEdge(input.TypeArguments[i], targetBase.TypeArguments[i], targetSubstitution: null, variance: VarianceKind.None, label: label);
                            break;
                        case VarianceKind.Out:
                            CreateCastEdge(input.TypeArguments[i], targetBase.TypeArguments[i], label);
                            break;
                        case VarianceKind.In:
                            CreateCastEdge(targetBase.TypeArguments[i], input.TypeArguments[i], label);
                            break;
                    }
                }
            } else if (input.Type is IArrayTypeSymbol && target.Type is IArrayTypeSymbol) {
                CreateCastEdge(input.TypeArguments.Single(), target.TypeArguments.Single(), label);
            } else if (input.Type is IPointerTypeSymbol && target.Type is IPointerTypeSymbol) {
                CreateCastEdge(input.TypeArguments.Single(), target.TypeArguments.Single(), label);
            }
        }

        private TypeWithNode conditionalAccessInstance;

        public override TypeWithNode VisitConditionalAccess(IConditionalAccessOperation operation, EdgeBuildingContext argument)
        {
            var oldConditionalAccessInstance = conditionalAccessInstance;
            try {
                var target = operation.Operation.Accept(this, EdgeBuildingContext.Normal);
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
            var target = operation.Target.Accept(this, EdgeBuildingContext.LValue);
            var value = operation.Value.Accept(this, EdgeBuildingContext.Normal);
            tsBuilder.CreateAssignmentEdge(source: value, target: target, new EdgeLabel("Assign", operation));
            return target;
        }

        public override TypeWithNode VisitEventAssignment(IEventAssignmentOperation operation, EdgeBuildingContext argument)
        {
            // event += value;
            var eventType = operation.EventReference.Accept(this, EdgeBuildingContext.LValue);
            var valueType = operation.HandlerValue.Accept(this, EdgeBuildingContext.Normal);
            // 'event += null;' is always allowed, even if the event isn't nullable
            eventType = eventType.WithNode(typeSystem.NullableNode);
            tsBuilder.CreateAssignmentEdge(source: valueType, target: eventType, new EdgeLabel("EventAssign", operation));
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitVariableDeclarationGroup(IVariableDeclarationGroupOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Children) {
                child.Accept(this, EdgeBuildingContext.Normal);
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
                    var init = decl.Initializer.Accept(this, EdgeBuildingContext.Normal);
                    localVarTypes.Add(decl.Symbol, init);
                    localVariables.Add(decl.Symbol);
                }
            } else {
                foreach (var child in operation.Children) {
                    child.Accept(this, EdgeBuildingContext.Normal);
                }
            }
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitVariableDeclarator(IVariableDeclaratorOperation operation, EdgeBuildingContext argument)
        {
            var variableType = typeSystem.GetSymbolType(operation.Symbol);
            if (operation.Initializer != null) {
                var init = operation.Initializer.Accept(this, EdgeBuildingContext.Normal);
                tsBuilder.CreateAssignmentEdge(source: init, target: variableType, new EdgeLabel("VarInit", operation));
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
                child.Accept(this, EdgeBuildingContext.Normal);
            }
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitMemberInitializer(IMemberInitializerOperation operation, EdgeBuildingContext argument)
        {
            var memberType = operation.InitializedMember.Accept(this, EdgeBuildingContext.LValue);
            Dereference(memberType, operation);
            memberType = memberType.WithNode(typeSystem.NonNullNode);
            var oldObjectCreationType = currentObjectCreationType;
            try {
                currentObjectCreationType = memberType;
                operation.Initializer?.Accept(this, EdgeBuildingContext.Normal);
            } finally {
                currentObjectCreationType = oldObjectCreationType;
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
            var lhs = operation.Target.Accept(this, EdgeBuildingContext.LValue);
            var rhs = operation.Value.Accept(this, EdgeBuildingContext.Normal);
            if (lhs.Type?.IsTupleType == true) {
                Debug.Assert(lhs.TypeArguments.Count == rhs.TypeArguments.Count);
                foreach (var (lhsElement, rhsElement) in lhs.TypeArguments.Zip(rhs.TypeArguments)) {
                    tsBuilder.CreateAssignmentEdge(rhsElement, lhsElement, new EdgeLabel("DeconstructionAssign", operation));
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
            TypeWithNode type;
            if (operation.Syntax is ObjectCreationExpressionSyntax oce) {
                type = oce.Type.Accept(syntaxVisitor).WithNode(typeSystem.NonNullNode);
            } else if (operation.Syntax is CastExpressionSyntax castSyntax) {
                type = castSyntax.Type.Accept(syntaxVisitor);
            } else {
                type = new TypeWithNode(operation.Type, typeSystem.NonNullNode, delegateType.FullTypeArguments().Select(tsBuilder.CreateTemporaryType).ToArray());
            }
            type.SetName("delegate");
            var substitution = new TypeSubstitution(type.TypeArguments, new TypeWithNode[0]);
            var delegateReturnType = typeSystem.GetSymbolType(delegateType.DelegateInvokeMethod.OriginalDefinition);
            delegateReturnType = delegateReturnType.WithSubstitution(delegateType.DelegateInvokeMethod.ReturnType, substitution);
            var delegateParameters = delegateType.DelegateInvokeMethod.Parameters
                .Select(p => typeSystem.GetSymbolType(p.OriginalDefinition).WithSubstitution(p.Type, substitution)).ToArray();
            switch (operation.Target) {
                case IAnonymousFunctionOperation lambda:
                    // Create edges for lambda parameters
                    IReadOnlyList<ParameterSyntax>? parameterList = null;
                    if (lambda.IsImplicit) {
                        foreach (var (lambdaParam, invokeParam) in lambda.Symbol.Parameters.Zip(delegateParameters)) {
                            // Lambda parameter generated by query expression
                            localVarTypes.Add(lambdaParam, invokeParam);
                            localVariables.Add(lambdaParam);
                        }
                    } else {
                        parameterList = lambda.Syntax switch
                        {
                            SimpleLambdaExpressionSyntax syntax => new[] { syntax.Parameter },
                            ParenthesizedLambdaExpressionSyntax lambdaSyntax => lambdaSyntax.ParameterList.Parameters,
                            AnonymousMethodExpressionSyntax syntax => (IReadOnlyList<ParameterSyntax>?)syntax.ParameterList?.Parameters,
                            _ => throw new NotImplementedException($"Unsupported syntax for lambdas: {lambda.Syntax}")
                        };
                    }
                    if (parameterList != null) {
                        Debug.Assert(parameterList.Count == delegateParameters.Length);
                        foreach (var (lambdaParamSyntax, invokeParam) in parameterList.Zip(delegateParameters)) {
                            if (lambdaParamSyntax.Type != null) {
                                var paramType = lambdaParamSyntax.Type.Accept(syntaxVisitor);
                                // C# 8 requires lambda parameters to exactly match the delegate type
                                // e.g. someEvent += delegate(object? sender, EventArgs? e)
                                // causes a warning that the EventArgs paramter must not be nullable.
                                // -> use VarianceKind.None.
                                tsBuilder.CreateTypeEdge(invokeParam, paramType, null, VarianceKind.None, new EdgeLabel("lambda parameter", lambdaParamSyntax));
                            } else {
                                // Implicitly typed lambda parameter: treat like a `var` variable initialization
                                var lambdaParamSymbol = syntaxVisitor.semanticModel.GetDeclaredSymbol(lambdaParamSyntax);
                                if (lambdaParamSymbol == null)
                                    throw new InvalidOperationException("Could not find symbol for lambda parameter");
                                localVarTypes.Add(lambdaParamSymbol, invokeParam);
                                localVariables.Add(lambdaParamSymbol);
                            }
                        }
                    }
                    // Analyze the body, and treat any `return` statements as assignments to `delegateReturnType`.
                    var outerMethodReturnType = syntaxVisitor.currentMethodReturnType;
                    try {
                        syntaxVisitor.currentMethodReturnType = delegateReturnType;
                        lambda.Body.Accept(this, EdgeBuildingContext.Normal);
                    } finally {
                        syntaxVisitor.currentMethodReturnType = outerMethodReturnType;
                    }
                    break;
                case IMethodReferenceOperation methodReference:
                    HandleMethodGroup(methodReference, delegateReturnType, delegateParameters);
                    break;
                default:
                    throw new NotImplementedException($"DelegateCreation with {operation.Target}");
            }
            return type;
        }

        public override TypeWithNode VisitTranslatedQuery(ITranslatedQueryOperation operation, EdgeBuildingContext argument)
        {
            return operation.Operation.Accept(this, argument);
        }
    }
}
