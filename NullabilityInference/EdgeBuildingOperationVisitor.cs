using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
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

        internal EdgeBuildingOperationVisitor(EdgeBuildingSyntaxVisitor syntaxVisitor, TypeSystem typeSystem)
        {
            this.syntaxVisitor = syntaxVisitor;
            this.typeSystem = typeSystem;
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

        public override TypeWithNode VisitBlock(IBlockOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Operations)
                child.Accept(this, argument);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitExpressionStatement(IExpressionStatementOperation operation, EdgeBuildingContext argument)
        {
            operation.Operation.Accept(this, argument);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitReturn(IReturnOperation operation, EdgeBuildingContext argument)
        {
            syntaxVisitor.CreateAssignmentEdge(
                source: operation.ReturnedValue.Accept(this, argument),
                target: syntaxVisitor.currentMethodReturnType)
                ?.SetLabel("return", operation.Syntax.GetLocation());
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitConditional(IConditionalOperation operation, EdgeBuildingContext argument)
        {
            operation.Condition.Accept(this, argument);
            var whenTrue = operation.WhenTrue.Accept(this, argument);
            var whenFalse = operation.WhenTrue.Accept(this, argument);
            Debug.Assert(whenTrue.Node.NullType == NullType.Oblivious);
            Debug.Assert(whenFalse.Node.NullType == NullType.Oblivious);
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitBinaryOperator(IBinaryOperation operation, EdgeBuildingContext argument)
        {
            if (operation.OperatorMethod != null)
                throw new NotImplementedException("Overloaded operator");
            var lhs = operation.LeftOperand.Accept(this, argument);
            var rhs = operation.RightOperand.Accept(this, argument);
            switch (operation.OperatorKind) {
                case BinaryOperatorKind.Equals:
                case BinaryOperatorKind.NotEquals:
                    return new TypeWithNode(operation.Type, typeSystem.ObliviousNode);
                default:
                    throw new NotImplementedException(operation.OperatorKind.ToString());
            }
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
            var variableType = typeSystem.GetSymbolType(operation.Local);
            if (syntaxVisitor.IsNonNullFlow(operation.Syntax)) {
                variableType = variableType.WithNode(typeSystem.NonNullNode);
            }
            return variableType;
        }

        public override TypeWithNode VisitParameterReference(IParameterReferenceOperation operation, EdgeBuildingContext argument)
        {
            var parameterType = typeSystem.GetSymbolType(operation.Parameter);
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
                default:
                    throw new NotImplementedException(operation.ReferenceKind.ToString());
            }
        }

        private void Dereference(TypeWithNode type, IOperation dereferencingOperation)
        {
            var edge = syntaxVisitor.CreateEdge(type.Node, typeSystem.NonNullNode);
            edge?.SetLabel("Deref", dereferencingOperation.Syntax.GetLocation());
        }

        public override TypeWithNode VisitFieldReference(IFieldReferenceOperation operation, EdgeBuildingContext argument)
        {
            if (operation.Instance != null) {
                Dereference(operation.Instance.Accept(this, argument), operation);
            }
            var fieldType = typeSystem.GetSymbolType(operation.Field);
            if (syntaxVisitor.IsNonNullFlow(operation.Syntax)) {
                fieldType = fieldType.WithNode(typeSystem.NonNullNode);
            }
            return fieldType;
        }

        public override TypeWithNode VisitInvocation(IInvocationOperation operation, EdgeBuildingContext argument)
        {
            if (operation.Instance != null) {
                Dereference(operation.Instance.Accept(this, argument), operation);
            }
            foreach (var arg in operation.Arguments)
                arg.Accept(this, argument);
            return typeSystem.GetSymbolType(operation.TargetMethod);
        }

        public override TypeWithNode VisitArgument(IArgumentOperation operation, EdgeBuildingContext argument)
        {
            var parameterType = typeSystem.GetSymbolType(operation.Parameter);
            var argumentType = operation.Value.Accept(this, argument);
            var edge = syntaxVisitor.CreateAssignmentEdge(source: argumentType, target: parameterType);
            edge?.SetLabel("Argument", operation.Syntax?.GetLocation());
            return argumentType;
        }

        public override TypeWithNode VisitObjectCreation(IObjectCreationOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Children)
                child.Accept(this, argument);
            if (operation.Syntax is ObjectCreationExpressionSyntax syntax) {
                return syntax.Type.Accept(syntaxVisitor);
            } else {
                throw new NotImplementedException($"ObjectCreationOperation with syntax={operation.Syntax}");
            }
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

        public override TypeWithNode VisitConversion(IConversionOperation operation, EdgeBuildingContext argument)
        {
            if (operation.OperatorMethod != null)
                throw new NotImplementedException("Overloaded conversion operator");
            // TODO: adjust return type
            return operation.Operand.Accept(this, argument);
        }

        public override TypeWithNode VisitSimpleAssignment(ISimpleAssignmentOperation operation, EdgeBuildingContext argument)
        {
            var target = operation.Target.Accept(this, argument);
            var value = operation.Value.Accept(this, argument);
            Debug.Assert(SymbolEqualityComparer.Default.Equals(target.Type, value.Type));
            var edge = syntaxVisitor.CreateAssignmentEdge(source: value, target: target);
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

        public override TypeWithNode VisitVariableDeclaration(IVariableDeclarationOperation operation, EdgeBuildingContext argument)
        {
            foreach (var child in operation.Children) {
                child.Accept(this, argument);
            }
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitVariableDeclarator(IVariableDeclaratorOperation operation, EdgeBuildingContext argument)
        {
            var variableType = typeSystem.GetSymbolType(operation.Symbol);
            if (operation.Initializer != null) {
                var init = operation.Initializer.Accept(this, argument);
                Debug.Assert(SymbolEqualityComparer.Default.Equals(variableType.Type, init.Type));
                var edge = syntaxVisitor.CreateAssignmentEdge(source: init, target: variableType);
                edge?.SetLabel("VarInit", operation.Syntax?.GetLocation());
            }
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitVariableInitializer(IVariableInitializerOperation operation, EdgeBuildingContext argument)
        {
            return operation.Value.Accept(this, argument);
        }
    }
}
