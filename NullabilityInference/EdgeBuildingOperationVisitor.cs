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
using System.Diagnostics.CodeAnalysis;
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
        /// Visiting an LValue for an assignments.
        /// In this mode, the flow state is ignored.
        /// </summary>
        LValue,
        /// <summary>
        /// Visiting a condition that will decide control-flow.
        /// In this mode, operations can create different flow-state onTrue/onFalse.
        /// </summary>
        Condition,
        /// <summary>
        /// Visiting descendants of a declaration expression. `var (a, b) = t;`
        /// </summary>
        DeclarationExpression
    }

    internal class EdgeBuildingOperationVisitor : OperationVisitor<EdgeBuildingContext, TypeWithNode>
    {
        private readonly EdgeBuildingSyntaxVisitor syntaxVisitor;
        private readonly TypeSystem typeSystem;
        private readonly TypeSystem.Builder tsBuilder;

        // Maps the known flow-state for variables
        private readonly FlowState flowState;

        // Used to return the separate flow-states in EdgeBuildingContext.Condition mode.
        private FlowState.Snapshot? flowStateReturnedOnTrue, flowStateReturnedOnFalse;

        internal EdgeBuildingOperationVisitor(EdgeBuildingSyntaxVisitor syntaxVisitor, TypeSystem typeSystem, TypeSystem.Builder tsBuilder)
        {
            this.syntaxVisitor = syntaxVisitor;
            this.typeSystem = typeSystem;
            this.tsBuilder = tsBuilder;
            this.flowState = new FlowState(typeSystem);
        }

        // Maps implicitly-typed local variables (or lambda parameters) to their inferred type.
        private readonly Dictionary<ISymbol, TypeWithNode> localVarTypes = new Dictionary<ISymbol, TypeWithNode>(SymbolEqualityComparer.Default);
        private readonly List<ISymbol> localVariables = new List<ISymbol>(); // used to remove dictionary entries at end of block

        public override TypeWithNode Visit(IOperation? operation, EdgeBuildingContext argument)
        {
            Debug.Assert(flowStateReturnedOnTrue == null && flowStateReturnedOnFalse == null,
                "flowStateReturned members should only be used when returning from Visit, never when entering");
            if (operation == null) {
                return new TypeWithNode(null, tsBuilder.ObliviousNode);
            }
            TypeWithNode result = operation.Accept(this, argument);
            if (result.Node == null) {
                // This happens with a "NoneOperation", because VisitNoneOperation() in the base class
                // returns default(TResult). Unfortunately that method is internal so we can't override it,
                // and instead have to handle it as a special case here.
                if (operation.Syntax?.Parent?.Kind() == SyntaxKind.PointerMemberAccessExpression
                    || operation.Syntax?.Kind() == SyntaxKind.PointerIndirectionExpression
                    || operation.Syntax?.Kind() == SyntaxKind.ElementAccessExpression) {
                    // https://github.com/dotnet/roslyn/issues/19960
                    var pointerOperation = operation.Children.First();
                    Debug.Assert(pointerOperation.Type?.TypeKind == TypeKind.Pointer);
                    var pointerType = Visit(pointerOperation, EdgeBuildingContext.Normal);
                    foreach (var child in operation.Children.Skip(1)) {
                        // e.g. index for ElementAccessExpression
                        child.Accept(this, EdgeBuildingContext.Normal);
                    }
                    return pointerType.TypeArguments.Single();
                }
                throw new NotSupportedException($"Unexpected null pointer in result.Node from {operation} at {operation.Syntax?.GetLocation().StartPosToString()}.");
            }
            return result;
        }

        private (FlowState.Snapshot onTrue, FlowState.Snapshot onFalse) VisitCondition(IOperation? operation)
        {
            Debug.Assert(flowStateReturnedOnTrue == null && flowStateReturnedOnFalse == null);
            operation?.Accept(this, EdgeBuildingContext.Condition);
            if (flowStateReturnedOnTrue != null && flowStateReturnedOnFalse != null) {
                var result = (flowStateReturnedOnTrue.Value, flowStateReturnedOnFalse.Value);
                flowStateReturnedOnTrue = null;
                flowStateReturnedOnFalse = null;
                return result;
            } else {
                Debug.Assert(flowStateReturnedOnTrue == null && flowStateReturnedOnFalse == null);
                var snapshot = flowState.SaveSnapshot();
                return (snapshot, snapshot);
            }
        }

        public override TypeWithNode DefaultVisit(IOperation operation, EdgeBuildingContext argument)
        {
            Debug.Fail($"Unhandled operation {operation.Kind} near {operation.Syntax?.GetLocation().StartPosToString()}");
            foreach (var child in operation.Children)
                child.Accept(this, EdgeBuildingContext.Normal);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitInvalid(IInvalidOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Children)
                child.Accept(this, EdgeBuildingContext.Normal);
            return typeSystem.GetObliviousType(operation.Type);
        }

        public override TypeWithNode VisitMethodBodyOperation(IMethodBodyOperation operation, EdgeBuildingContext argument)
        {
            var oldFlowState = flowState.SaveSnapshot();
            try {
                flowState.Clear();
                foreach (var child in operation.Children)
                    child.Accept(this, EdgeBuildingContext.Normal);
                return typeSystem.VoidType;
            } finally {
                flowState.RestoreSnapshot(oldFlowState);
            }
        }

        public override TypeWithNode VisitConstructorBodyOperation(IConstructorBodyOperation operation, EdgeBuildingContext argument)
        {
            var oldFlowState = flowState.SaveSnapshot();
            try {
                flowState.Clear();
                foreach (var child in operation.Children)
                    child.Accept(this, EdgeBuildingContext.Normal);
                return typeSystem.VoidType;
            } finally {
                flowState.RestoreSnapshot(oldFlowState);
            }
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

        public override TypeWithNode VisitLocalFunction(ILocalFunctionOperation operation, EdgeBuildingContext argument)
        {
            var oldFlowState = flowState.SaveSnapshot();
            try {
                using var outerMethod = syntaxVisitor.SaveCurrentMethod();
                syntaxVisitor.currentMethod = operation.Symbol;
                syntaxVisitor.currentMethodReturnType = syntaxVisitor.GetMethodReturnSymbol(operation.Symbol);
                flowState.Clear();
                foreach (var child in operation.Children)
                    child.Accept(this, EdgeBuildingContext.Normal);
                return typeSystem.VoidType;
            } finally {
                flowState.RestoreSnapshot(oldFlowState);
            }
        }

        public override TypeWithNode VisitSwitch(ISwitchOperation operation, EdgeBuildingContext argument)
        {
            operation.Value.Accept(this, EdgeBuildingContext.Normal);
            var onBodyStart = flowState.SaveSnapshot();
            flowState.MakeUnreachable();
            var onBodyEnd = flowState.SaveSnapshot();
            foreach (var child in operation.Cases) {
                flowState.RestoreSnapshot(onBodyStart);
                child.Accept(this, EdgeBuildingContext.Normal);
                flowState.JoinWith(onBodyEnd, tsBuilder, new EdgeLabel("End of switch", operation));
                onBodyEnd = flowState.SaveSnapshot();
            }
            OnLabel(operation.ExitLabel);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitSwitchCase(ISwitchCaseOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Body)
                child.Accept(this, EdgeBuildingContext.Normal);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitSwitchExpression(ISwitchExpressionOperation operation, EdgeBuildingContext argument)
        {
            var returnType = tsBuilder.CreateHelperType(operation.Type);
            returnType.SetName("switch expr");
            var switchValue = Visit(operation.Value, EdgeBuildingContext.Normal);
            var onBodyStart = flowState.SaveSnapshot();
            flowState.MakeUnreachable();
            var onBodyEnd = flowState.SaveSnapshot();
            foreach (var arm in operation.Arms) {
                flowState.RestoreSnapshot(onBodyStart);
                var pattern = Visit(arm.Pattern, EdgeBuildingContext.Normal);
                PerformPatternMatch(switchValue, pattern, arm);

                if (arm.Guard != null) {
                    var (onTrue, onFalse) = VisitCondition(arm.Guard);
                    flowState.RestoreSnapshot(onTrue);
                    onBodyEnd = JoinFlowSnapshots(onBodyEnd, onFalse, new EdgeLabel("pattern guard false", arm));
                }

                var value = Visit(arm.Value, EdgeBuildingContext.Normal);
                tsBuilder.CreateAssignmentEdge(value, returnType, new EdgeLabel("switch expr arm", arm));

                flowState.JoinWith(onBodyEnd, tsBuilder, new EdgeLabel("end of switch expr arm", arm));
                onBodyEnd = flowState.SaveSnapshot();
            }
            flowState.RestoreSnapshot(onBodyEnd);
            return returnType;
        }

        public override TypeWithNode VisitTry(ITryOperation operation, EdgeBuildingContext argument)
        {
            operation.Body.Accept(this, EdgeBuildingContext.Normal);
            // Accurately tracking flow-state with exception handling is tricky, we simplify things by resetting
            // all our knowledge at the beginning of each catch handler.
            foreach (var child in operation.Catches) {
                flowState.Clear();
                child.Accept(this, EdgeBuildingContext.Normal);
            }
            flowState.Clear();
            operation.Finally?.Accept(this, EdgeBuildingContext.Normal);
            OnLabel(operation.ExitLabel);
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
            foreach (var child in operation.Before)
                child.Accept(this, EdgeBuildingContext.Normal);
            flowState.Clear();
            var (onTrue, onFalse) = VisitCondition(operation.Condition);
            flowState.RestoreSnapshot(onTrue);
            operation.Body.Accept(this, EdgeBuildingContext.Normal);
            OnLabel(operation.ContinueLabel);
            foreach (var child in operation.AtLoopBottom)
                child.Accept(this, EdgeBuildingContext.Normal);
            flowState.RestoreSnapshot(onFalse);
            OnLabel(operation.ExitLabel);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitWhileLoop(IWhileLoopOperation operation, EdgeBuildingContext argument)
        {
            if (operation.ConditionIsTop) {
                // while(cond) body;
                flowState.Clear();
                OnLabel(operation.ContinueLabel);
                var (onTrue, onFalse) = VisitCondition(operation.Condition);
                flowState.RestoreSnapshot(onTrue);
                operation.Body.Accept(this, EdgeBuildingContext.Normal);
                flowState.RestoreSnapshot(onFalse);
                OnLabel(operation.ExitLabel);
            } else {
                // do body; while(cond);
                flowState.Clear();
                operation.Body.Accept(this, EdgeBuildingContext.Normal);
                OnLabel(operation.ContinueLabel);
                var (onTrue, onFalse) = VisitCondition(operation.Condition);
                flowState.RestoreSnapshot(onFalse);
                OnLabel(operation.ExitLabel);
            }
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
                collection = Visit(arrayOperand, EdgeBuildingContext.Normal);
                Dereference(collection, arrayOperand);
                elementType = collection.TypeArguments.Single();
            } else {
                collection = Visit(operation.Collection, EdgeBuildingContext.Normal);
                Dereference(collection, operation.Collection);

                // Determine the enumerator type (which might have nullabilities dependent on type arguments from the collection type)
                if (loopInfo.GetEnumeratorMethod == null)
                    throw new NotSupportedException("foreach loop without GetEnumeratorMethod");
                var getEnumeratorSubstitution = SubstitutionForMemberAccess(collection, loopInfo.GetEnumeratorMethod);
                var enumeratorType = typeSystem.GetSymbolType(loopInfo.GetEnumeratorMethod.OriginalDefinition);
                enumeratorType = enumeratorType.WithSubstitution(loopInfo.GetEnumeratorMethod.ReturnType, getEnumeratorSubstitution, tsBuilder);

                // Determine the element type (which might have nullabilities dependent on type arguments from the enumerator type)
                if (loopInfo.CurrentProperty == null)
                    throw new NotSupportedException("foreach loop without CurrentProperty");
                var getCurrentSubstitution = SubstitutionForMemberAccess(enumeratorType, loopInfo.CurrentProperty);
                elementType = typeSystem.GetSymbolType(loopInfo.CurrentProperty.OriginalDefinition);
                elementType = elementType.WithSubstitution(loopInfo.CurrentProperty.Type, getCurrentSubstitution, tsBuilder);
            }

            flowState.Clear();

            var loopVariable = Visit(operation.LoopControlVariable, EdgeBuildingContext.LValue);
            CreateConversionEdge(elementType, loopVariable, loopInfo.ElementConversion, new EdgeLabel("loop variable", operation));
            operation.Body.Accept(this, EdgeBuildingContext.Normal);

            flowState.Clear();
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitAwait(IAwaitOperation operation, EdgeBuildingContext argument)
        {
            var awaitableType = Visit(operation.Operation, EdgeBuildingContext.Normal);
            var awaitInfo = syntaxVisitor.semanticModel.GetAwaitExpressionInfo((AwaitExpressionSyntax)operation.Syntax);

            if (awaitInfo.GetAwaiterMethod == null) {
                // Not sure why, but sometimes roslyn fails to give us any AwaitExpressionInfo.
                return typeSystem.GetObliviousType(operation.Type);
            }
            var getAwaiterSubstitution = SubstitutionForMemberAccess(awaitableType, awaitInfo.GetAwaiterMethod);
            var awaiterType = typeSystem.GetSymbolType(awaitInfo.GetAwaiterMethod.OriginalDefinition);
            awaiterType = awaiterType.WithSubstitution(awaitInfo.GetAwaiterMethod.ReturnType, getAwaiterSubstitution, tsBuilder);

            if (awaitInfo.GetResultMethod == null)
                throw new NotSupportedException("await without GetResultMethod");
            var getResultSubstitution = SubstitutionForMemberAccess(awaiterType, awaitInfo.GetResultMethod);
            var resultType = typeSystem.GetSymbolType(awaitInfo.GetResultMethod.OriginalDefinition);
            resultType = resultType.WithSubstitution(awaitInfo.GetResultMethod.ReturnType, getResultSubstitution, tsBuilder);

            return resultType;

        }

        public override TypeWithNode VisitBranch(IBranchOperation operation, EdgeBuildingContext argument)
        {
            // goto / break / continue
            flowState.MakeUnreachable();
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitLabeled(ILabeledOperation operation, EdgeBuildingContext argument)
        {
            OnLabel(operation.Label);
            if (operation.Operation != null)
                operation.Operation.Accept(this, argument);
            return typeSystem.VoidType;
        }

        private void OnLabel(ILabelSymbol? symbol)
        {
            if (symbol == null)
                return;
            flowState.Clear();
        }

        public override TypeWithNode VisitUsing(IUsingOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Children)
                child.Accept(this, EdgeBuildingContext.Normal);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitUsingDeclaration(IUsingDeclarationOperation operation, EdgeBuildingContext argument)
        {
            // using var x = y;
            operation.DeclarationGroup.Accept(this, EdgeBuildingContext.Normal);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitLock(ILockOperation operation, EdgeBuildingContext argument)
        {
            var monitor = Visit(operation.LockedValue, EdgeBuildingContext.Normal);
            Dereference(monitor, operation.LockedValue);
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
            CreateEdgeForReturnValue(operation);
            switch (operation.Kind) {
                case OperationKind.Return:
                case OperationKind.YieldBreak:
                    flowState.MakeUnreachable();
                    break;
                case OperationKind.YieldReturn:
                    break;
                default:
                    throw new NotSupportedException(operation.Kind.ToString());
            }
            return typeSystem.VoidType;
        }

        private void CreateEdgeForReturnValue(IReturnOperation operation)
        {
            if (operation.ReturnedValue == null) {
                return;
            }
            var returnType = syntaxVisitor.currentMethodReturnType;
            if (operation.Kind == OperationKind.Return && returnType.Type?.SpecialType == SpecialType.System_Boolean && syntaxVisitor.currentMethod != null) {
                // When returning a boolean, export flow-states for out parameters
                var (onTrue, onFalse) = VisitCondition(operation.ReturnedValue);
                foreach (var param in syntaxVisitor.currentMethod.Parameters) {
                    if (!typeSystem.TryGetOutParameterFlowNodes(param, out var outNodes))
                        continue;
                    var path = new AccessPath(AccessPathRoot.Local, ImmutableArray.Create<ISymbol>(param));
                    if (!onTrue.Unreachable) {
                        flowState.RestoreSnapshot(onTrue);
                        if (!flowState.TryGetNode(path, out var node)) {
                            node = typeSystem.GetSymbolType(param, ignoreAttributes: true).Node;
                        }
                        tsBuilder.CreateEdge(node, outNodes.whenTrue, new EdgeLabel($"flow-state of {param.Name} on return true", operation));
                    }
                    if (!onFalse.Unreachable) {
                        flowState.RestoreSnapshot(onFalse);
                        if (!flowState.TryGetNode(path, out var node)) {
                            node = typeSystem.GetSymbolType(param, ignoreAttributes: true).Node;
                        }
                        tsBuilder.CreateEdge(node, outNodes.whenFalse, new EdgeLabel($"flow-state of {param.Name} on return false", operation));
                    }
                }
                // no need to create edge for return value, as `bool` is a value-type
                return;
            }
            var returnVal = Visit(operation.ReturnedValue, EdgeBuildingContext.Normal);
            if (operation.Kind == OperationKind.YieldReturn) {
                if (returnType.TypeArguments.Count == 0) {
                    // returning non-generic enumerable -> no edge needed
                    return;
                }
                returnType = returnType.TypeArguments.Single();
            }
            tsBuilder.CreateAssignmentEdge(
                source: returnVal,
                target: returnType,
                label: new EdgeLabel("return", operation));
        }

        public override TypeWithNode VisitConditional(IConditionalOperation operation, EdgeBuildingContext argument)
        {
            var (onTrue, onFalse) = VisitCondition(operation.Condition);

            var mergedType = tsBuilder.CreateHelperType(operation.Type);
            mergedType.SetName("?:");

            flowState.RestoreSnapshot(onTrue);
            var whenTrue = Visit(operation.WhenTrue, argument);
            tsBuilder.CreateAssignmentEdge(whenTrue, mergedType, new EdgeLabel("then", operation.WhenTrue));
            var flowAfterTrue = flowState.SaveSnapshot();

            flowState.RestoreSnapshot(onFalse);
            if (operation.WhenFalse != null) {
                var whenFalse = Visit(operation.WhenFalse, argument);
                tsBuilder.CreateAssignmentEdge(whenFalse, mergedType, new EdgeLabel("else", operation.WhenFalse));
            }
            flowState.JoinWith(flowAfterTrue, tsBuilder, new EdgeLabel("End of conditional", operation));

            return mergedType;
        }

        public override TypeWithNode VisitUnaryOperator(IUnaryOperation operation, EdgeBuildingContext argument)
        {
            if (argument == EdgeBuildingContext.Condition && operation.OperatorMethod == null && operation.OperatorKind == UnaryOperatorKind.Not) {
                var (onTrue, onFalse) = VisitCondition(operation.Operand);
                (flowStateReturnedOnFalse, flowStateReturnedOnTrue) = (onTrue, onFalse);
                return typeSystem.GetObliviousType(operation.Type);
            }
            var operand = Visit(operation.Operand, EdgeBuildingContext.Normal);
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
            if (operation.OperatorKind == BinaryOperatorKind.ConditionalAnd || operation.OperatorKind == BinaryOperatorKind.ConditionalOr) {
                Debug.Assert(operation.OperatorMethod == null);
                // logical operator does not always evaluate the RHS, so we need to be careful with the flow-state
                var (onTrue, onFalse) = VisitCondition(operation.LeftOperand);
                FlowState.Snapshot onShortCircuit;
                if (operation.OperatorKind == BinaryOperatorKind.ConditionalAnd) {
                    flowState.RestoreSnapshot(onTrue); // && evaluates the RHS only if the lhs is true
                    onShortCircuit = onFalse;
                } else {
                    Debug.Assert(operation.OperatorKind == BinaryOperatorKind.ConditionalOr);
                    flowState.RestoreSnapshot(onFalse); // && evaluates the RHS only if the lhs is false
                    onShortCircuit = onTrue;
                }
                if (argument == EdgeBuildingContext.Condition) {
                    var (rhsTrue, rhsFalse) = VisitCondition(operation.RightOperand);
                    if (operation.OperatorKind == BinaryOperatorKind.ConditionalAnd) {
                        flowStateReturnedOnTrue = rhsTrue;
                        flowStateReturnedOnFalse = JoinFlowSnapshots(onFalse, rhsFalse, new EdgeLabel("&& false", operation));
                    } else {
                        Debug.Assert(operation.OperatorKind == BinaryOperatorKind.ConditionalOr);
                        flowStateReturnedOnTrue = JoinFlowSnapshots(onTrue, rhsTrue, new EdgeLabel("|| true", operation));
                        flowStateReturnedOnFalse = rhsFalse;
                    }
                } else {
                    Visit(operation.RightOperand, EdgeBuildingContext.Normal);
                    flowState.JoinWith(onShortCircuit, tsBuilder, new EdgeLabel("short-circuit", operation));
                }
                return typeSystem.GetObliviousType(operation.Type);
            }
            var lhs = Visit(operation.LeftOperand, EdgeBuildingContext.Normal);
            var rhs = Visit(operation.RightOperand, EdgeBuildingContext.Normal);
            if (operation.OperatorMethod != null) {
                return HandleOverloadedBinaryOperator(operation, lhs, rhs, operation.OperatorMethod, operation.IsLifted);
            }
            if (operation.OperatorKind == BinaryOperatorKind.NotEquals || operation.OperatorKind == BinaryOperatorKind.Equals) {
                if (IsNullLiteral(operation.RightOperand) && AccessPath.FromOperation(operation.LeftOperand) is AccessPath leftPath) {
                    if (leftPath.IsParameter) {
                        // check for 'Debug.Assert(someParam != null);' and similar constructs
                        HandleNullAssert(operation, lhs, valueOnNull: operation.OperatorKind == BinaryOperatorKind.Equals);
                    }
                    if (argument == EdgeBuildingContext.Condition) {
                        SetFlowStateForNullCheck(leftPath, operation.OperatorKind);
                    }
                } else if (IsNullLiteral(operation.LeftOperand) && AccessPath.FromOperation(operation.RightOperand) is AccessPath rightPath) {
                    if (rightPath.IsParameter) {
                        // check for 'Debug.Assert(someParam != null);' and similar constructs
                        HandleNullAssert(operation, rhs, valueOnNull: operation.OperatorKind == BinaryOperatorKind.Equals);
                    }
                    if (argument == EdgeBuildingContext.Condition) {
                        SetFlowStateForNullCheck(rightPath, operation.OperatorKind);
                    }
                }
            }
            return typeSystem.GetObliviousType(operation.Type);
        }

        /// <summary>
        /// Set the flow state after a "path op null" check.
        /// Sets flowStateReturnedOnTrue/flowStateReturnedOnFalse, only call this method in EdgeBuildingContext.Condition!
        /// </summary>
        private void SetFlowStateForNullCheck(AccessPath path, BinaryOperatorKind operatorKind)
        {
            flowState.SetNode(path, typeSystem.NonNullNode, clearMembers: false);
            var flowStateOnNotNull = flowState.SaveSnapshot();
            flowState.SetNode(path, typeSystem.NullableNode, clearMembers: true);
            var flowStateOnNull = flowState.SaveSnapshot();

            if (operatorKind == BinaryOperatorKind.Equals) { // path == null
                flowStateReturnedOnTrue = flowStateOnNull;
                flowStateReturnedOnFalse = flowStateOnNotNull;
            } else {
                Debug.Assert(operatorKind == BinaryOperatorKind.NotEquals); // path != null
                flowStateReturnedOnTrue = flowStateOnNotNull;
                flowStateReturnedOnFalse = flowStateOnNull;
            }
        }

        private FlowState.Snapshot JoinFlowSnapshots(FlowState.Snapshot a, FlowState.Snapshot b, EdgeLabel label)
        {
            FlowState s = new FlowState(typeSystem);
            s.RestoreSnapshot(a);
            s.JoinWith(b, tsBuilder, label);
            return s.SaveSnapshot();
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

        public override TypeWithNode VisitTupleBinaryOperator(ITupleBinaryOperation operation, EdgeBuildingContext argument)
        {
            // tuple comparison
            Visit(operation.LeftOperand, EdgeBuildingContext.Normal);
            Visit(operation.RightOperand, EdgeBuildingContext.Normal);
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
            if (operation.Parent is IArgumentOperation { Parameter: { } param } && param.HasDoesNotReturnIfAttribute(out bool parameterValue)) {
                if (parameterValue == valueOnNull) {
                    tsBuilder.CreateEdge(testedNode.Node, typeSystem.NonNullNode, new EdgeLabel("DoesNotReturnIf on param", operation));
                }
            }
        }

        public override TypeWithNode VisitCompoundAssignment(ICompoundAssignmentOperation operation, EdgeBuildingContext argument)
        {
            var lhs = Visit(operation.Target, EdgeBuildingContext.LValue);
            var rhs = Visit(operation.Value, EdgeBuildingContext.Normal);
            if (operation.OperatorMethod != null) {
                var operatorResult = HandleOverloadedBinaryOperator(operation, lhs, rhs, operation.OperatorMethod, operation.IsLifted);
                CreateConversionEdge(operatorResult, lhs, operation.GetOutConversion(), new EdgeLabel("compound assign", operation));
                if (AccessPath.FromOperation(operation.Target) is AccessPath path) {
                    flowState.SetNode(path, operatorResult.Node, clearMembers: true);
                }
                return lhs;
            }
            if (operation.Type?.TypeKind == TypeKind.Delegate && operation.OperatorKind == BinaryOperatorKind.Add) {
                tsBuilder.CreateAssignmentEdge(rhs.WithNode(typeSystem.ObliviousNode), lhs, new EdgeLabel("delegate combine", operation));
                return lhs;
            } else {
                return typeSystem.GetObliviousType(operation.Type);
            }
        }

        public override TypeWithNode VisitCoalesce(ICoalesceOperation operation, EdgeBuildingContext argument)
        {
            var lhs = Visit(operation.Value, EdgeBuildingContext.Normal);
            var flowAfterLHS = flowState.SaveSnapshot();
            var rhs = Visit(operation.WhenNull, EdgeBuildingContext.Normal);
            var result = tsBuilder.CreateHelperType(operation.Type);
            result.SetName("??");
            CreateCastEdge(lhs, result, new EdgeLabel("lhs of ??", operation));
            tsBuilder.CreateAssignmentEdge(rhs, result, new EdgeLabel("rhs of ??", operation));
            flowState.JoinWith(flowAfterLHS, tsBuilder, new EdgeLabel("join after ??"));
            // for the top-level nullability, only the rhs is relevant
            return result.WithNode(rhs.Node);
        }

        public override TypeWithNode VisitThrow(IThrowOperation operation, EdgeBuildingContext argument)
        {
            if (operation.Exception != null) {
                var exception = Visit(operation.Exception, EdgeBuildingContext.Normal);
                Dereference(exception, operation.Exception);
            }
            flowState.MakeUnreachable();
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitLocalReference(ILocalReferenceOperation operation, EdgeBuildingContext argument)
        {
            if (argument == EdgeBuildingContext.DeclarationExpression) {
                var ty = tsBuilder.CreateHelperType(operation.Type);
                ty.SetName(operation.Local.Name);

                localVarTypes.Add(operation.Local, ty);
                localVariables.Add(operation.Local);
                return ty;
            }
            if (!localVarTypes.TryGetValue(operation.Local, out TypeWithNode variableType)) {
                variableType = typeSystem.GetSymbolType(operation.Local);
            }
            if (argument != EdgeBuildingContext.LValue && TryGetFlowState(operation, out var flowNode, out var flowLabel)) {
                variableType = variableType.WithFlowState(flowNode, flowLabel);
            }
            return variableType;
        }

        public override TypeWithNode VisitParameterReference(IParameterReferenceOperation operation, EdgeBuildingContext argument)
        {
            if (!localVarTypes.TryGetValue(operation.Parameter, out TypeWithNode parameterType)) {
                parameterType = typeSystem.GetSymbolType(operation.Parameter);
            }
            if (argument != EdgeBuildingContext.LValue && TryGetFlowState(operation, out var flowNode, out var flowLabel)) {
                parameterType = parameterType.WithFlowState(flowNode, flowLabel);
            }
            return parameterType;
        }

        private bool TryGetFlowState(IOperation operation, [NotNullWhen(true)] out NullabilityNode? flowNode, out string? flowLabel)
        {
            if (syntaxVisitor.IsNonNullFlow(operation.Syntax)) {
                flowNode = typeSystem.NonNullNode;
                flowLabel = "(not-null via roslyn)";
                return true;
            } else if (AccessPath.FromOperation(operation) is AccessPath path) {
                if (flowState.TryGetNode(path, out flowNode)) {
#if DEBUG
                    flowLabel = $"(flow-state of '{path}')";
#else
                    flowLabel = "(via flow-state)";
#endif
                    return true;
                }
                flowLabel = null;
                return false;
            } else {
                flowNode = null;
                flowLabel = null;
                return false;
            }
        }

        public override TypeWithNode VisitInstanceReference(IInstanceReferenceOperation operation, EdgeBuildingContext argument)
        {
            switch (operation.ReferenceKind) {
                case InstanceReferenceKind.ContainingTypeInstance:
                    return typeSystem.GetObliviousType(operation.Type).WithNode(typeSystem.NonNullNode);
                case InstanceReferenceKind.ImplicitReceiver:
                    return currentObjectCreationType;
                case InstanceReferenceKind.PatternInput:
                    return currentPatternInput;
                default:
                    throw new NotImplementedException(operation.ReferenceKind.ToString());
            }
        }

        private void Dereference(TypeWithNode? type, IOperation dereferencedOperation)
        {
            if (type != null) {
                tsBuilder.CreateEdge(type.Value.Node, typeSystem.NonNullNode, new EdgeLabel("Deref", dereferencedOperation));
            }
            if (AccessPath.FromOperation(dereferencedOperation) is AccessPath path) {
                flowState.SetNode(path, typeSystem.NonNullNode, clearMembers: false);
            }
        }

        private IReadOnlyList<TypeWithNode> ClassTypeArgumentsForMemberAccess(TypeWithNode? receiverType, ISymbol member)
        {
            if (receiverType != null) {
                receiverType = typeSystem.GetBaseType(receiverType.Value, member.ContainingType);
            }
            if (receiverType == null) {
                receiverType = typeSystem.GetObliviousType(member.ContainingType);
            }
            return receiverType.Value.TypeArguments;
        }

        internal TypeSubstitution SubstitutionForMemberAccess(TypeWithNode? receiverType, ISymbol member)
        {
            var classTypeArguments = ClassTypeArgumentsForMemberAccess(receiverType, member);
            TypeWithNode[] methodTypeArguments;
            if (member is IMethodSymbol method) {
                methodTypeArguments = new TypeWithNode[method.FullArity()];
                int i = 0;
                foreach (var ta in method.FullTypeArguments()) {
                    methodTypeArguments[i++] = typeSystem.GetObliviousType(ta);
                }
            } else {
                methodTypeArguments = new TypeWithNode[0];
            }
            return new TypeSubstitution(classTypeArguments, methodTypeArguments);
        }

        public override TypeWithNode VisitFieldReference(IFieldReferenceOperation operation, EdgeBuildingContext argument)
        {
            IFieldSymbol field = operation.Field;
            field = field.CorrespondingTupleField ?? field;
            // field.OriginalDefinition only works as expected when we're using the underlying tuple field

            TypeWithNode? receiverType = GetReceiverType(operation);
            var substitution = SubstitutionForMemberAccess(receiverType, field);
            var fieldType = typeSystem.GetSymbolType(field.OriginalDefinition);
            fieldType = fieldType.WithSubstitution(field.Type, substitution, tsBuilder);
            if (argument != EdgeBuildingContext.LValue && TryGetFlowState(operation, out var flowNode, out var flowLabel)) {
                fieldType = fieldType.WithFlowState(flowNode, flowLabel);
            }
            return fieldType;
        }

        public override TypeWithNode VisitPropertyReference(IPropertyReferenceOperation operation, EdgeBuildingContext argument)
        {
            TypeWithNode? receiverType = GetReceiverType(operation);
            var substitution = SubstitutionForMemberAccess(receiverType, operation.Property);
            HandleArguments(substitution, operation.Arguments, invocationContext: argument);
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
                propertyType = propertyType.WithSubstitution(operation.Property.Type, substitution, tsBuilder);
            }
            if (argument != EdgeBuildingContext.LValue && TryGetFlowState(operation, out var flowNode, out var flowLabel)) {
                propertyType = propertyType.WithFlowState(flowNode, flowLabel);
            }
            return propertyType;
        }

        public override TypeWithNode VisitEventReference(IEventReferenceOperation operation, EdgeBuildingContext argument)
        {
            TypeWithNode? receiverType = GetReceiverType(operation);
            var substitution = SubstitutionForMemberAccess(receiverType, operation.Event);
            var eventType = typeSystem.GetSymbolType(operation.Event.OriginalDefinition);
            eventType = eventType.WithSubstitution(operation.Event.Type, substitution, tsBuilder);
            return eventType;
        }

        private TypeWithNode? GetReceiverType(IMemberReferenceOperation operation)
        {
            if (operation.Instance != null) {
                var receiverType = Visit(operation.Instance, EdgeBuildingContext.Normal);
                Dereference(receiverType, operation.Instance);
                return receiverType;
            } else if (operation.Syntax is MemberAccessExpressionSyntax { Expression: var receiverSyntax }) {
                // Look for a syntactic type as in "SomeClass<T>.SomeMember"
                return receiverSyntax.Accept(syntaxVisitor);
            } else {
                return null;
            }
        }

        public override TypeWithNode VisitInvocation(IInvocationOperation operation, EdgeBuildingContext argument)
        {
            TypeWithNode? receiverType = null;
            if (operation.Instance != null) {
                receiverType = Visit(operation.Instance, EdgeBuildingContext.Normal);
                Dereference(receiverType, operation.Instance);
            } else if (operation.Syntax is InvocationExpressionSyntax { Expression: MemberAccessExpressionSyntax { Expression: var receiverSyntax } } invocation) {
                // Look for a syntactic type as in "SomeClass<T>.StaticMethod();"
                // However this might also be a call to an extension method "someExpr.ExtensionMethod()".
                // In this case we don't want to visit the 'this' argument expression twice (once via receiverSyntax,
                // then again via operation.Arguments[0]).
                if (!syntaxVisitor.IsReducedExtensionMethodCall(invocation)) {
                    receiverType = receiverSyntax.Accept(syntaxVisitor);
                }
            }

            var targetMethod = operation.TargetMethod;
            var classTypeArgNodes = ClassTypeArgumentsForMemberAccess(receiverType, targetMethod);
            TypeWithNode[]? methodTypeArgNodes = null;
            if (operation.Syntax is InvocationExpressionSyntax ies) {
                var typeArgSyntax = FindTypeArgumentList(ies.Expression);
                methodTypeArgNodes = typeArgSyntax?.Arguments.Select(syntaxVisitor.Visit).ToArray();
            }
            methodTypeArgNodes = ExtendMethodTypeArguments(targetMethod, methodTypeArgNodes);
            var substitution = new TypeSubstitution(classTypeArgNodes, methodTypeArgNodes);
            var argumentTypes = HandleArguments(substitution, operation.Arguments, invocationContext: argument);
            var returnType = typeSystem.GetSymbolType(targetMethod.OriginalDefinition);
            returnType = returnType.WithSubstitution(targetMethod.ReturnType, substitution, tsBuilder);

            if (typeSystem.GetNotNullIfNotNullParam(operation.TargetMethod.OriginalDefinition) is { } notNullParam) {
                returnType = returnType.WithNode(argumentTypes[notNullParam.Ordinal].Node);
            }
            if (operation.TargetMethod.HasDoesNotReturnAttribute()) {
                flowState.MakeUnreachable();
            }

            return returnType;
        }

        private TypeWithNode[] ExtendMethodTypeArguments(IMethodSymbol targetMethod, TypeWithNode[]? methodTypeArgNodes)
        {
            // If there are no syntactic type arguments, create temporary type nodes instead to represent
            // the inferred type arguments.
            if (methodTypeArgNodes == null) {
                methodTypeArgNodes = targetMethod.TypeArguments.Select(tsBuilder.CreateHelperType).ToArray();
                for (int i = 0; i < methodTypeArgNodes.Length; i++) {
                    methodTypeArgNodes[i].SetName($"{targetMethod.Name}!!{i}");
                }
            }
            if (targetMethod.ContainingSymbol is IMethodSymbol outerMethod) {
                // Adjust the array to include entries to type parameters from outer methods
                int outerArity = outerMethod.FullArity();
                if (outerArity > 0) {
                    TypeWithNode[] fullMethodTypeArgNodes = new TypeWithNode[methodTypeArgNodes.Length + outerArity];
                    int i = 0;
                    foreach (var ta in outerMethod.FullTypeArguments()) {
                        fullMethodTypeArgNodes[i++] = typeSystem.GetObliviousType(ta);
                    }
                    Debug.Assert(i + methodTypeArgNodes.Length == fullMethodTypeArgNodes.Length);
                    Array.Copy(methodTypeArgNodes, 0, fullMethodTypeArgNodes, i, methodTypeArgNodes.Length);
                    return fullMethodTypeArgNodes;
                }
            }
            return methodTypeArgNodes;
        }

        private void HandleMethodGroup(IMethodReferenceOperation operation, TypeWithNode delegateReturnType, List<TypeWithNode> delegateParameters)
        {
            TypeWithNode? receiverType;
            if (operation.Instance != null && operation.Method.IsExtensionMethod) {
                receiverType = null;
                delegateParameters.Insert(0, Visit(operation.Instance, EdgeBuildingContext.Normal));
            } else {
                receiverType = GetReceiverType(operation);
            }
            var classTypeArgNodes = ClassTypeArgumentsForMemberAccess(receiverType, operation.Method);
            TypeWithNode[]? methodTypeArgNodes = null;
            if (operation.Syntax is ExpressionSyntax es) {
                var typeArgSyntax = FindTypeArgumentList(es);
                methodTypeArgNodes = typeArgSyntax?.Arguments.Select(syntaxVisitor.Visit).ToArray();
            }
            methodTypeArgNodes = ExtendMethodTypeArguments(operation.Method, methodTypeArgNodes);
            var substitution = new TypeSubstitution(classTypeArgNodes, methodTypeArgNodes);
            EdgeLabel label = new EdgeLabel($"MethodGroup", operation);

            Debug.Assert(operation.Method.Parameters.Length == delegateParameters.Count);
            foreach (var (methodParam, delegateParam) in operation.Method.Parameters.Zip(delegateParameters)) {
                var methodParamType = typeSystem.GetSymbolType(methodParam.OriginalDefinition);
                methodParamType = methodParamType.WithSubstitution(methodParam.Type, substitution, tsBuilder);
                switch (methodParam.RefKind.ToVariance()) {
                    case VarianceKind.In:
                        CreateCastEdge(delegateParam, methodParamType, label);
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
            returnType = returnType.WithSubstitution(operation.Method.ReturnType, substitution, tsBuilder);
            CreateCastEdge(returnType, delegateReturnType, label);
        }

        private TypeArgumentListSyntax? FindTypeArgumentList(ExpressionSyntax expr) => expr switch {
            GenericNameSyntax gns => gns.TypeArgumentList,
            MemberAccessExpressionSyntax maes => FindTypeArgumentList(maes.Name),
            AliasQualifiedNameSyntax aqns => FindTypeArgumentList(aqns.Name),
            _ => null,
        };

        private List<TypeWithNode> HandleArguments(TypeSubstitution substitution, ImmutableArray<IArgumentOperation> arguments, EdgeBuildingContext invocationContext)
        {
            List<TypeWithNode> argumentTypes = new List<TypeWithNode>();
            Action? afterCall = null;
            FlowState? flowStateOnTrue = null;
            FlowState? flowStateOnFalse = null;
            foreach (var arg in arguments) {
                if (arg.Parameter == null) {
                    throw new NotImplementedException("__arglist not supported");
                }
                var param = arg.Parameter.OriginalDefinition;
                var parameterType = typeSystem.GetSymbolType(param);
                bool isLValue = param.RefKind == RefKind.Ref || param.RefKind == RefKind.Out;
                TypeWithNode argumentType;
                if (!isLValue && param.Type.SpecialType == SpecialType.System_Boolean && param.HasDoesNotReturnIfAttribute(out bool doesNotReturnOn)) {
                    var (onArgTrue, onArgFalse) = VisitCondition(arg.Value);
                    // only keep the flow-state from the variant where the function returns
                    flowState.RestoreSnapshot(doesNotReturnOn ? onArgFalse : onArgTrue);
                    argumentType = new TypeWithNode(param.Type, typeSystem.ObliviousNode); // bool
                } else {
                    argumentType = Visit(arg.Value, isLValue ? EdgeBuildingContext.LValue : EdgeBuildingContext.Normal);
                }
                argumentTypes.Add(argumentType);
                // Create an assignment edge from argument to parameter.
                // We use the parameter's original type + substitution so that a type parameter `T` appearing in
                // multiple parameters uses the same nullability nodes for all occurrences.
                var variance = (param.RefKind.ToVariance(), VarianceKind.In).Combine();
                tsBuilder.CreateTypeEdge(source: argumentType, target: parameterType, substitution, variance, new EdgeLabel("Argument", arg));
                if (isLValue && AccessPath.FromRefArgument(arg.Value) is AccessPath path) {
                    // Processing of the flow-state is delayed until after all arguments were visited.
                    afterCall += delegate {
                        if (invocationContext == EdgeBuildingContext.Condition) {
                            if (flowStateOnTrue == null || flowStateOnFalse == null) {
                                // split flow-state based on return value
                                var snapshot = flowState.SaveSnapshot();
                                flowStateOnTrue = new FlowState(typeSystem);
                                flowStateOnFalse = new FlowState(typeSystem);
                                flowStateOnTrue.RestoreSnapshot(snapshot);
                                flowStateOnFalse.RestoreSnapshot(snapshot);
                            }
                            if (param.RefKind == RefKind.Out) {
                                // set flow-state to value provided by the method
                                var (whenTrue, whenFalse) = typeSystem.GetOutParameterFlowNodes(param, substitution);
                                flowStateOnTrue.SetNode(path, whenTrue, clearMembers: true);
                                flowStateOnFalse.SetNode(path, whenFalse, clearMembers: true);
                            } else {
                                // the method might have mutated the by-ref argument -> reset flow-state to argument's declared type
                                flowStateOnTrue.SetNode(path, argumentType.Node, clearMembers: true);
                                flowStateOnFalse.SetNode(path, argumentType.Node, clearMembers: true);
                            }
                        } else {
                            if (param.RefKind == RefKind.Out) {
                                // set flow-state to value provided by the method
                                flowState.SetNode(path, parameterType.Node, clearMembers: true);
                            } else {
                                // the method might have mutated the by-ref argument -> reset flow-state to argument's declared type
                                flowState.SetNode(path, argumentType.Node, clearMembers: true);
                            }
                        }
                    };
                }
            }
            afterCall?.Invoke();
            if (invocationContext == EdgeBuildingContext.Condition) {
                flowStateReturnedOnTrue = flowStateOnTrue?.SaveSnapshot();
                flowStateReturnedOnFalse = flowStateOnFalse?.SaveSnapshot();
            } else {
                Debug.Assert(flowStateOnTrue == null && flowStateOnFalse == null);
                Debug.Assert(flowStateReturnedOnTrue == null && flowStateReturnedOnFalse == null);
            }
            return argumentTypes;
        }

        private TypeWithNode currentObjectCreationType;

        public override TypeWithNode VisitObjectCreation(IObjectCreationOperation operation, EdgeBuildingContext argument)
        {
            TypeWithNode newObjectType;
            if (operation.Syntax is ObjectCreationExpressionSyntax syntax) {
                newObjectType = syntax.Type.Accept(syntaxVisitor).WithNode(typeSystem.NonNullNode);
            } else if (operation.Syntax is ImplicitObjectCreationExpressionSyntax) {
                newObjectType = tsBuilder.CreateHelperType(operation.Type);
            } else {
                throw new NotImplementedException($"ObjectCreationOperation with syntax={operation.Syntax}");
            }
            var substitution = new TypeSubstitution(newObjectType.TypeArguments, new TypeWithNode[0]);
            HandleArguments(substitution, operation.Arguments, invocationContext: argument);

            var oldObjectCreationType = currentObjectCreationType;
            try {
                currentObjectCreationType = newObjectType;
                operation.Initializer?.Accept(this, EdgeBuildingContext.Normal);
            } finally {
                currentObjectCreationType = oldObjectCreationType;
            }
            return newObjectType;
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
            var newObjectType = tsBuilder.CreateHelperType(operation.Type);
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
                arrayType = tsBuilder.CreateHelperType(operation.Type);
            } else if (operation.Syntax is ArrayCreationExpressionSyntax syntax) {
                arrayType = syntax.Type.Accept(syntaxVisitor);
            } else if (operation.Syntax is ImplicitArrayCreationExpressionSyntax) {
                // implicitly-typed 'new[] { ... }'
                arrayType = tsBuilder.CreateHelperType(operation.Type);
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
            var arrayType = Visit(operation.ArrayReference, EdgeBuildingContext.Normal);
            Dereference(arrayType, operation.ArrayReference);
            foreach (var index in operation.Indices) {
                index.Accept(this, EdgeBuildingContext.Normal);
            }
            return arrayType.TypeArguments.Single();
        }

        private void HandleArrayInitializer(IArrayInitializerOperation operation, TypeWithNode arrayType)
        {
            TypeWithNode elementType;
            if (arrayType.Type is IArrayTypeSymbol arrayTypeSym && arrayTypeSym.Rank > 1) {
                var lowerDimArrayType = typeSystem.Compilation.CreateArrayTypeSymbol(arrayTypeSym.ElementType, arrayTypeSym.Rank - 1);
                elementType = new TypeWithNode(lowerDimArrayType, typeSystem.ObliviousNode, arrayType.TypeArguments);
            } else {
                elementType = arrayType.TypeArguments.Single();
            }
            foreach (var elementInit in operation.ElementValues) {
                if (elementInit is IArrayInitializerOperation nestedArrayInit) {
                    HandleArrayInitializer(nestedArrayInit, elementType);
                } else {
                    var initType = Visit(elementInit, EdgeBuildingContext.Normal);
                    tsBuilder.CreateAssignmentEdge(source: initType, target: elementType, new EdgeLabel("ArrayInit", elementInit));
                }
            }
        }

        public override TypeWithNode VisitPropertyInitializer(IPropertyInitializerOperation operation, EdgeBuildingContext argument)
        {
            var property = operation.InitializedProperties.Single();
            var propertyType = typeSystem.GetSymbolType(property);
            var value = Visit(operation.Value, EdgeBuildingContext.Normal);
            tsBuilder.CreateAssignmentEdge(source: value, target: propertyType, new EdgeLabel("PropertyInit", operation));
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitFieldInitializer(IFieldInitializerOperation operation, EdgeBuildingContext argument)
        {
            var field = operation.InitializedFields.Single();
            var fieldType = typeSystem.GetSymbolType(field);
            var value = Visit(operation.Value, EdgeBuildingContext.Normal);
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
            if (argument == EdgeBuildingContext.Condition && operation.ConstantValue.Value is bool booleanValue) {
                var snapshot = flowState.SaveSnapshot();
                if (booleanValue) {
                    flowStateReturnedOnTrue = snapshot;
                    flowStateReturnedOnFalse = snapshot.WithUnreachable();
                } else {
                    flowStateReturnedOnTrue = snapshot.WithUnreachable();
                    flowStateReturnedOnFalse = snapshot;
                }
                return new TypeWithNode(operation.Type, typeSystem.ObliviousNode); // bool
            }
            if (operation.Type?.IsValueType == true) {
                return new TypeWithNode(operation.Type, typeSystem.ObliviousNode);
            } else if (operation.ConstantValue.HasValue && operation.ConstantValue.Value == null) {
                if (operation.Type != null) {
                    return typeSystem.GetObliviousType(operation.Type).WithNode(typeSystem.NullableNode);
                } else {
                    return new TypeWithNode(operation.Type, typeSystem.NullableNode);
                }
            } else {
                return new TypeWithNode(operation.Type, typeSystem.NonNullNode);
            }
        }

        public override TypeWithNode VisitDefaultValue(IDefaultValueOperation operation, EdgeBuildingContext argument)
        {
            var type = typeSystem.GetObliviousType(operation.Type);
            if (operation.Type?.IsReferenceType ?? false) {
                type = type.WithNode(typeSystem.NullableNode);
            }
            return type;
        }

        public override TypeWithNode VisitInterpolatedString(IInterpolatedStringOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Children)
                child.Accept(this, EdgeBuildingContext.Normal);
            return new TypeWithNode(operation.Type, typeSystem.NonNullNode);
        }

        public override TypeWithNode VisitInterpolatedStringText(IInterpolatedStringTextOperation operation, EdgeBuildingContext argument)
        {
            return Visit(operation.Text, argument);
        }

        public override TypeWithNode VisitInterpolation(IInterpolationOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Children) {
                child.Accept(this, EdgeBuildingContext.Normal);
            }
            return typeSystem.GetObliviousType(operation.Type);
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
            var input = Visit(operation.Operand, EdgeBuildingContext.Normal);
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
                targetType = tsBuilder.CreateHelperType(operation.Type);
                targetType.SetName($"{conv}Conversion");
            }
            CreateConversionEdge(input, targetType, conv, new EdgeLabel($"{conv}Conversion", operation));
            return targetType;
        }

        private void CreateConversionEdge(TypeWithNode input, TypeWithNode target, Conversion conv, EdgeLabel label)
        {
            if (conv.IsUserDefined) {
                var method = conv.MethodSymbol;
                if (method == null)
                    throw new NotSupportedException("User-defined conversion without MethodSymbol");
                Debug.Assert(method.FullArity() == 0);
                TypeWithNode[] classTypeArguments = method.ContainingType.FullTypeArguments().Select(tsBuilder.CreateHelperType).ToArray();
                var substitution = new TypeSubstitution(classTypeArguments, new TypeWithNode[0]);

                var param = method.Parameters.Single();
                var paramType = typeSystem.GetSymbolType(param.OriginalDefinition);
                paramType = paramType.WithSubstitution(param.Type, substitution, tsBuilder);
                CreateCastEdge(input, paramType, label);

                // use return type of user-defined operator as input for the remaining conversion
                var returnType = typeSystem.GetSymbolType(method.OriginalDefinition);
                returnType = returnType.WithSubstitution(method.ReturnType, substitution, tsBuilder);
                CreateCastEdge(returnType, target, label);
            } else if (conv.IsReference || conv.IsIdentity || conv.IsBoxing || conv.IsPointer) {
                CreateCastEdge(input, target, label);
            } else if (conv.IsUnboxing) {
                if (target.Type?.IsSystemNullable() == true) {
                    CreateCastEdge(input, target.WithNode(typeSystem.NullableNode), label);
                } else {
                    // unboxing to a non-nullable type requires a non-null box
                    CreateCastEdge(input, target.WithNode(typeSystem.NonNullNode), label);
                }
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
            } else if (conv.IsNumeric || conv.IsConstantExpression || conv.IsEnumeration || conv.IsIntPtr) {
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
                throw new NotImplementedException($"Unknown conversion: {conv}, label={label}");
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
                && typeSystem.GetBaseType(input, namedTargetType) is TypeWithNode inputBase) {
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
                 && typeSystem.GetBaseType(target, namedInputType) is TypeWithNode targetBase) {
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
                var target = Visit(operation.Operation, EdgeBuildingContext.Normal);
                if (target.Type.IsSystemNullable()) {
                    conditionalAccessInstance = target.TypeArguments.Single();
                } else {
                    conditionalAccessInstance = target.WithNode(typeSystem.NonNullNode);
                }
                var value = Visit(operation.WhenNotNull, argument);
                if (operation.Type.IsSystemNullable()) {
                    if (value.Type.IsSystemNullable())
                        return value;
                    else
                        return new TypeWithNode(operation.Type, typeSystem.NullableNode, new[] { value });
                } else {
                    return value.WithNode(typeSystem.NullableNode);
                }
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
            var target = Visit(operation.Target, EdgeBuildingContext.LValue);
            var value = Visit(operation.Value, EdgeBuildingContext.Normal);
            if (AccessPath.FromOperation(operation.Target) is AccessPath targetPath) {
                flowState.SetNode(targetPath, value.Node, clearMembers: true);
            }
            tsBuilder.CreateAssignmentEdge(source: value, target: target, new EdgeLabel("Assign", operation));
            return target;
        }

        public override TypeWithNode VisitCoalesceAssignment(ICoalesceAssignmentOperation operation, EdgeBuildingContext argument)
        {
            // target ??= value;
            var target = Visit(operation.Target, EdgeBuildingContext.LValue);
            var flowStateAfterLHS = flowState.SaveSnapshot();
            var value = Visit(operation.Value, EdgeBuildingContext.Normal);
            tsBuilder.CreateAssignmentEdge(source: value, target: target, new EdgeLabel("??= Assign", operation));
            flowState.JoinWith(flowStateAfterLHS, tsBuilder, new EdgeLabel("??= short-circuit", operation));
            if (AccessPath.FromOperation(operation.Target) is AccessPath targetPath) {
                flowState.SetNode(targetPath, value.Node, clearMembers: true);
            }
            return target;
        }

        public override TypeWithNode VisitEventAssignment(IEventAssignmentOperation operation, EdgeBuildingContext argument)
        {
            // event += value;
            var eventType = Visit(operation.EventReference, EdgeBuildingContext.LValue);
            var valueType = Visit(operation.HandlerValue, EdgeBuildingContext.Normal);
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

        public override TypeWithNode VisitVariableDeclaration(IVariableDeclarationOperation operation, EdgeBuildingContext argument)
        {
            if (operation.Syntax is VariableDeclarationSyntax { Type: SimpleNameSyntax { IsVar: true } }) {
                // Implicitly typed local variable.
                // We syntactically can't use "var?", an implicitly typed variable is
                // implicitly always declared as nullable (and can be non-nullable only due to its
                // flow-state).
                foreach (var decl in operation.Declarators) {
                    var init = Visit(decl.Initializer, EdgeBuildingContext.Normal);

                    // But because our own flow-state isn't tracked perfectly but resets to the declared state
                    // under some circumstances (e.g. loops), we use a helper node instead of typeSystem.NullableNode.
                    var helperNode = tsBuilder.CreateHelperNode();
                    helperNode.SetName(decl.Symbol.Name);
                    tsBuilder.CreateEdge(init.Node, helperNode, new EdgeLabel("var init", decl));

                    flowState.SetNode(new AccessPath(AccessPathRoot.Local, ImmutableArray.Create<ISymbol>(decl.Symbol)), init.Node, clearMembers: true);
                    localVarTypes.Add(decl.Symbol, init.WithNode(helperNode));
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
                var init = Visit(operation.Initializer, EdgeBuildingContext.Normal);
                tsBuilder.CreateAssignmentEdge(source: init, target: variableType, new EdgeLabel("VarInit", operation));

                flowState.SetNode(new AccessPath(AccessPathRoot.Local, ImmutableArray.Create<ISymbol>(operation.Symbol)), init.Node, clearMembers: true);
            }
            return variableType;
        }

        public override TypeWithNode VisitVariableInitializer(IVariableInitializerOperation operation, EdgeBuildingContext argument)
        {
            return Visit(operation.Value, argument);
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
            var memberType = Visit(operation.InitializedMember, EdgeBuildingContext.LValue);
            Dereference(memberType, operation.InitializedMember);
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
            var elementTypes = operation.Elements.Select(e => Visit(e, argument)).ToArray();
            return new TypeWithNode(operation.Type, typeSystem.ObliviousNode, elementTypes);
        }

        public override TypeWithNode VisitDeconstructionAssignment(IDeconstructionAssignmentOperation operation, EdgeBuildingContext argument)
        {
            var lhs = Visit(operation.Target, EdgeBuildingContext.LValue);
            var rhs = Visit(operation.Value, EdgeBuildingContext.Normal);
            if (lhs.Type?.IsTupleType == true) {
                var rhsTypes = rhs.Type?.IsTupleType == true ? rhs.TypeArguments : GetDeconstructParameters(operation, rhs.TypeArguments, lhs.TypeArguments.Count);
                Debug.Assert(lhs.TypeArguments.Count == rhsTypes.Count);
                foreach (var (lhsElement, rhsElement) in lhs.TypeArguments.Zip(rhsTypes)) {
                    tsBuilder.CreateAssignmentEdge(rhsElement, lhsElement, new EdgeLabel("DeconstructionAssign", operation));
                }
                if (AccessPath.FromOperation(operation.Target) is AccessPath path) {
                    // clear flow-state from individual tuple members, if any
                    flowState.SetNode(path, typeSystem.ObliviousNode, clearMembers: true);
                }
                return rhs;
            } else {
                throw new NotImplementedException("Could not deconstruct into non-tuple type near " + operation.Syntax?.GetLocation().StartPosToString());
            }
        }

        private IReadOnlyCollection<TypeWithNode> GetDeconstructParameters(IDeconstructionAssignmentOperation deconstructOperation, IReadOnlyList<TypeWithNode> typeArguments, int parameterCount)
        {
            var deconstructMethodSymbol = GetDeconstructorMethod(deconstructOperation, parameterCount);
            var substitution = new TypeSubstitution(typeArguments, new TypeWithNode[0]);
            return GetGenericParametersSymbolTypes(deconstructMethodSymbol.Parameters, substitution);
        }

        private IReadOnlyCollection<TypeWithNode> GetGenericParametersSymbolTypes(ImmutableArray<IParameterSymbol> parameters, TypeSubstitution substitution)
            => parameters.Select(p => typeSystem.GetSymbolType(p.OriginalDefinition).WithSubstitution(p.Type, substitution, tsBuilder)).ToArray();

        /// <remarks>
        /// Request for full version of this to be added to the Roslyn API: https://github.com/dotnet/roslyn/issues/33590
        /// </remarks>
        private static IMethodSymbol GetDeconstructorMethod(IDeconstructionAssignmentOperation deconstructOperation, int parameterCount)
        {
            return deconstructOperation.Value.Type?.GetMembers("Deconstruct").OfType<IMethodSymbol>()
                .FirstOrDefault(m => m.Parameters.Length == parameterCount)
                ?? throw new NotImplementedException("Could not find deconstruct method for operation near " + deconstructOperation.Syntax?.GetLocation().StartPosToString());
        }

        public override TypeWithNode VisitDeclarationExpression(IDeclarationExpressionOperation operation, EdgeBuildingContext argument)
        {
            // appears e.g. in `var (a, b) = tuple;`
            Debug.Assert(argument == EdgeBuildingContext.LValue);
            return Visit(operation.Expression, EdgeBuildingContext.DeclarationExpression);
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
                type = new TypeWithNode(operation.Type, typeSystem.NonNullNode, delegateType.FullTypeArguments().Select(tsBuilder.CreateHelperType).ToArray());
            }
            type.SetName("delegate");
            var substitution = new TypeSubstitution(type.TypeArguments, new TypeWithNode[0]);
            var delegateReturnType = typeSystem.GetSymbolType(delegateType.DelegateInvokeMethod.OriginalDefinition);
            delegateReturnType = delegateReturnType.WithSubstitution(delegateType.DelegateInvokeMethod.ReturnType, substitution, tsBuilder);
            var delegateParameters = delegateType.DelegateInvokeMethod.Parameters
                .Select(p => typeSystem.GetSymbolType(p.OriginalDefinition).WithSubstitution(p.Type, substitution, tsBuilder)).ToList();
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
                        parameterList = lambda.Syntax switch {
                            SimpleLambdaExpressionSyntax syntax => new[] { syntax.Parameter },
                            ParenthesizedLambdaExpressionSyntax lambdaSyntax => lambdaSyntax.ParameterList.Parameters,
                            AnonymousMethodExpressionSyntax syntax => (IReadOnlyList<ParameterSyntax>?)syntax.ParameterList?.Parameters,
                            _ => throw new NotImplementedException($"Unsupported syntax for lambdas: {lambda.Syntax}")
                        };
                    }
                    if (parameterList != null) {
                        Debug.Assert(parameterList.Count == delegateParameters.Count);
                        Debug.Assert(parameterList.Count == lambda.Symbol.Parameters.Length);
                        foreach (var ((lambdaParamSyntax, invokeParam), lambdaParamSymbol) in parameterList.Zip(delegateParameters).Zip(lambda.Symbol.Parameters)) {
                            if (lambdaParamSyntax.Type != null) {
                                var paramType = lambdaParamSyntax.Type.Accept(syntaxVisitor);
                                localVarTypes.Add(lambdaParamSymbol, paramType);
                                localVariables.Add(lambdaParamSymbol);
                                // C# 8 requires lambda parameters to exactly match the delegate type
                                // e.g. someEvent += delegate(object? sender, EventArgs? e)
                                // causes a warning that the EventArgs paramter must not be nullable.
                                // -> use VarianceKind.None.
                                tsBuilder.CreateTypeEdge(invokeParam, paramType, null, VarianceKind.None, new EdgeLabel("lambda parameter", lambdaParamSyntax));
                            } else {
                                // Implicitly typed lambda parameter: treat like a `var` variable initialization
                                localVarTypes.Add(lambdaParamSymbol, invokeParam);
                                localVariables.Add(lambdaParamSymbol);
                            }
                        }
                    }
                    // Analyze the body, and treat any `return` statements as assignments to `delegateReturnType`.
                    var outerFlowState = flowState.SaveSnapshot();
                    try {
                        using var outerMethod = syntaxVisitor.SaveCurrentMethod();
                        syntaxVisitor.currentMethod = lambda.Symbol;
                        if (lambda.Symbol.IsAsync) {
                            syntaxVisitor.currentMethodReturnType = syntaxVisitor.ExtractTaskReturnType(delegateReturnType);
                        } else {
                            syntaxVisitor.currentMethodReturnType = delegateReturnType;
                        }
                        flowState.Clear();
                        lambda.Body.Accept(this, EdgeBuildingContext.Normal);
                    } finally {
                        flowState.RestoreSnapshot(outerFlowState);
                    }
                    break;
                case IMethodReferenceOperation methodReference:
                    HandleMethodGroup(methodReference, delegateReturnType, delegateParameters);
                    break;
                default:
                    throw new NotImplementedException($"DelegateCreation with {operation.Target} near {operation.Syntax?.GetLocation().StartPosToString()}");
            }
            return type;
        }

        public override TypeWithNode VisitTranslatedQuery(ITranslatedQueryOperation operation, EdgeBuildingContext argument)
        {
            return Visit(operation.Operation, argument);
        }

        public override TypeWithNode VisitIsPattern(IIsPatternOperation operation, EdgeBuildingContext argument)
        {
            var value = Visit(operation.Value, EdgeBuildingContext.Normal);
            var pattern = Visit(operation.Pattern, EdgeBuildingContext.Normal);
            PerformPatternMatch(value, pattern, operation);
            return typeSystem.GetObliviousType(operation.Type);
        }

        private void PerformPatternMatch(TypeWithNode value, TypeWithNode pattern, IOperation operation)
        {
            CreateCastEdge(value, pattern, new EdgeLabel("match", operation));
        }

        public override TypeWithNode VisitDeclarationPattern(IDeclarationPatternOperation operation, EdgeBuildingContext argument)
        {
            if (operation.DeclaredSymbol != null) {
                var symbolType = typeSystem.GetSymbolType(operation.DeclaredSymbol);
                if (!operation.MatchesNull) {
                    symbolType = symbolType.WithNode(typeSystem.ObliviousNode);
                }
                return symbolType;
            } else {
                return typeSystem.GetObliviousType(operation.Type);
            }
        }

        public override TypeWithNode VisitTypePattern(ITypePatternOperation operation, EdgeBuildingContext argument)
        {
            return typeSystem.GetObliviousType(operation.Type);
        }

        public override TypeWithNode VisitDiscardPattern(IDiscardPatternOperation operation, EdgeBuildingContext argument)
        {
            return typeSystem.GetObliviousType(operation.Type);
        }

        private TypeWithNode currentPatternInput;

        public override TypeWithNode VisitRecursivePattern(IRecursivePatternOperation operation, EdgeBuildingContext argument)
        {
            var outerPatternInput = currentPatternInput;
            try {
                if (operation.Syntax is RecursivePatternSyntax { Type: { } typeSyntax }) {
                    currentPatternInput = typeSyntax.Accept(syntaxVisitor);
                } else {
                    currentPatternInput = tsBuilder.CreateHelperType(operation.Type);
                    currentPatternInput.SetName("RecursivePattern");
                }
                // Recursive pattern never matches null, so ignore the top-level nullability.
                currentPatternInput = currentPatternInput.WithNode(typeSystem.ObliviousNode);
                foreach (var child in operation.Children)
                    child.Accept(this, EdgeBuildingContext.Normal);
                return currentPatternInput;
            } finally {
                currentPatternInput = outerPatternInput;
            }
        }

        public override TypeWithNode VisitPropertySubpattern(IPropertySubpatternOperation operation, EdgeBuildingContext argument)
        {
            var value = Visit(operation.Member, EdgeBuildingContext.Normal);
            var pattern = Visit(operation.Pattern, EdgeBuildingContext.Normal);
            PerformPatternMatch(value, pattern, operation);
            return typeSystem.GetObliviousType(operation.Type);
        }

        public override TypeWithNode VisitConstantPattern(IConstantPatternOperation operation, EdgeBuildingContext argument)
        {
            return typeSystem.GetObliviousType(operation.Type);
        }

        public override TypeWithNode VisitRelationalPattern(IRelationalPatternOperation operation, EdgeBuildingContext argument)
        {
            Visit(operation.Value, EdgeBuildingContext.Normal);
            return typeSystem.GetObliviousType(operation.Type);
        }

        public override TypeWithNode VisitNegatedPattern(INegatedPatternOperation operation, EdgeBuildingContext argument)
        {
            Visit(operation.Pattern, EdgeBuildingContext.Normal);
            return typeSystem.GetObliviousType(operation.Type);
        }

        public override TypeWithNode VisitBinaryPattern(IBinaryPatternOperation operation, EdgeBuildingContext argument)
        {
            Visit(operation.LeftPattern, EdgeBuildingContext.Normal);
            Visit(operation.RightPattern, EdgeBuildingContext.Normal);
            return typeSystem.GetObliviousType(operation.Type);
        }

        public override TypeWithNode VisitAddressOf(IAddressOfOperation operation, EdgeBuildingContext argument)
        {
            Visit(operation.Reference, EdgeBuildingContext.LValue);
            return typeSystem.GetObliviousType(operation.Type);
        }

        public override TypeWithNode VisitSizeOf(ISizeOfOperation operation, EdgeBuildingContext argument)
        {
            return typeSystem.GetObliviousType(operation.Type);
        }
    }
}
