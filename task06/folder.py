from model import *


class ConstantFolder():
    def visit_number(self, number):
        return Number(number.value)

    def visit_function(self, function):
        return Function(function.args,
                        [stmt.accept(self) for stmt in function.body])

    def visit_function_definition(self, function_definition):
        return FunctionDefinition(function_definition.name,
                                  function_definition.function.accept(self))

    def visit_conditional(self, conditional):
        return Conditional(
            conditional.condition.accept(self),
            [stmt.accept(self) for stmt in conditional.if_true],
            [stmt.accept(self) for stmt in conditional.if_false])

    def visit_print(self, print_):
        return Print(print_.expr.accept(self))

    def visit_read(self, read):
        return Read(read.name)

    def visit_function_call(self, function_call):
        return FunctionCall(
               function_call.fun_expr.accept(self),
               [stmt.accept(self) for stmt in function_call.args])

    def visit_reference(self, reference):
        return Reference(reference.name)

    def visit_binary_operation(self, binary_operation):
        lhs = binary_operation.lhs.accept(self)
        op = binary_operation.op
        rhs = binary_operation.rhs.accept(self)
        s = Scope()
        if isinstance(lhs, Number) and isinstance(rhs, Number):
            return BinaryOperation(lhs, op, rhs).evaluate(s)
        if (isinstance(lhs, Number) and lhs.value == 0 and
                isinstance(rhs, Reference) and op == '*'):
            return Number(0)
        if (isinstance(rhs, Number) and rhs.value == 0 and
                isinstance(lhs, Reference) and op == '*'):
            return Number(0)
        if (isinstance(lhs, Reference) and isinstance(rhs, Reference) and
                lhs.name == rhs.name and op == '-'):
            return Number(0)
        return BinaryOperation(lhs, op, rhs)

    def visit_unary_operation(self, unary_operation):
        expr = unary_operation.expr.accept(self)
        op = unary_operation.op
        s = Scope()
        if isinstance(expr, Number):
            return UnaryOperation(op, expr).evaluate(s)
        return UnaryOperation(op, expr)
