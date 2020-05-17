// Copyright (c) 2020 Daniel Grunwald

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

        public override TypeWithNode VisitBranch(IBranchOperation operation, EdgeBuildingContext argument)
        {
            // goto / break / continue
            foreach (var child in operation.Children)
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
            if (operation.ReturnedValue != null) {
                var returnVal = operation.ReturnedValue.Accept(this, argument);
                var edge = syntaxVisitor.CreateAssignmentEdge(
                    source: returnVal,
                    target: syntaxVisitor.currentMethodReturnType);
                edge?.SetLabel("return", operation.Syntax.GetLocation());
            }
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitConditional(IConditionalOperation operation, EdgeBuildingContext argument)
        {
            operation.Condition.Accept(this, argument);
            var whenTrue = operation.WhenTrue.Accept(this, argument);
            var whenFalse = operation.WhenFalse?.Accept(this, argument);
            Debug.Assert(whenTrue.Node.NullType == NullType.Oblivious);
            Debug.Assert(!whenFalse.HasValue || whenFalse.Value.Node.NullType == NullType.Oblivious);
            return typeSystem.VoidType;
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
            return typeSystem.GetObliviousType(operation.Type);
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
            var parameterType = typeSystem.GetSymbolType(operation.Parameter.OriginalDefinition);
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
                var edge = syntaxVisitor.CreateEdge(type.Value.Node, typeSystem.NonNullNode);
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
            methodTypeArgNodes ??= operation.TargetMethod.TypeArguments.Select(syntaxVisitor.CreateTemporaryType).ToArray();
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
                VarianceKind variance = param.RefKind switch {
                    // The direction of the edge depends on the refkind:
                    RefKind.None => VarianceKind.Out, // argument --> parameter
                    RefKind.In => VarianceKind.Out,   // argument --> parameter
                    RefKind.Ref => VarianceKind.None, // argument <-> parameter
                    RefKind.Out => VarianceKind.In,   // argument <-- parameter
                    _ => throw new NotSupportedException($"RefKind unsupported: {param.RefKind}")
                };
                var edge = syntaxVisitor.CreateTypeEdge(source: argumentType, target: parameterType, substitution, variance);
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
                var edge = syntaxVisitor.CreateAssignmentEdge(source: initType, target: elementType);
                edge?.SetLabel("ArrayInit", elementInit.Syntax?.GetLocation());
            }
        }

        public override TypeWithNode VisitPropertyInitializer(IPropertyInitializerOperation operation, EdgeBuildingContext argument)
        {
            var property = operation.InitializedProperties.Single();
            var propertyType = typeSystem.GetSymbolType(property);
            var value = operation.Value.Accept(this, argument);
            var edge = syntaxVisitor.CreateAssignmentEdge(source: value, target: propertyType);
            edge?.SetLabel("PropertyInit", operation.Syntax?.GetLocation());
            return typeSystem.VoidType;
        }

        public override TypeWithNode VisitFieldInitializer(IFieldInitializerOperation operation, EdgeBuildingContext argument)
        {
            var field = operation.InitializedFields.Single();
            var fieldType = typeSystem.GetSymbolType(field);
            var value = operation.Value.Accept(this, argument);
            var edge = syntaxVisitor.CreateAssignmentEdge(source: value, target: fieldType);
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
            } else if (conv.IsReference) {
                TypeWithNode targetType;
                if (conv.IsExplicit && operation.Syntax is CastExpressionSyntax cast) {
                    targetType = cast.Type.Accept(syntaxVisitor);
                } else {
                    targetType = syntaxVisitor.CreateTemporaryType(operation.Type);
                    targetType.SetName("ImplicitReferenceConversion");
                }
                // TODO: handle type arguments
                var edge = syntaxVisitor.CreateEdge(source: input.Node, target: targetType.Node);
                edge?.SetLabel("Cast", operation.Syntax?.GetLocation());
                return targetType;
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

        private readonly Dictionary<ILocalSymbol, TypeWithNode> localVarTypes = new Dictionary<ILocalSymbol, TypeWithNode>();
        private readonly List<ILocalSymbol> localVariables = new List<ILocalSymbol>(); // used to remove dictionary entries at end of block

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
                var edge = syntaxVisitor.CreateAssignmentEdge(source: init, target: variableType);
                edge?.SetLabel("VarInit", operation.Syntax?.GetLocation());
            }
            return typeSystem.VoidType;
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
    }
}
