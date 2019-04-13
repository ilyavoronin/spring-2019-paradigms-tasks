from model import *


class ConstantFolder(ASTNodeVisitor):
    def visit_number(self, number):
        return number

    def visit_function(self, function):
        return Function(function.args,
                        [stmt.accept(self) for stmt in function.body])

    def visit_function_definition(self, function_definition):
        return FunctionDefinition(function_definition.name,
                                  function_definition.function.accept(self))

    def visit_conditional(self, conditional):
        return Conditional(
            conditional.condition.accept(self),
            [stmt.accept(self) for stmt in conditional.if_true or []],
            [stmt.accept(self) for stmt in conditional.if_false or []])

    def visit_print(self, print_):
        return Print(print_.expr.accept(self))

    def visit_read(self, read):
        return read

    def visit_function_call(self, function_call):
        return FunctionCall(
               function_call.fun_expr.accept(self),
               [arg.accept(self) for arg in function_call.args])

    def visit_reference(self, reference):
        return reference

    def visit_binary_operation(self, binary_operation):
        lhs = binary_operation.lhs.accept(self)
        op = binary_operation.op
        rhs = binary_operation.rhs.accept(self)
        if isinstance(lhs, Number) and isinstance(rhs, Number):
            return BinaryOperation(lhs, op, rhs).evaluate(Scope())
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
        if isinstance(expr, Number):
            return UnaryOperation(op, expr).evaluate(Scope())
        return UnaryOperation(op, expr)


def fold_constants(program):
    folder = ConstantFolder()
    return program.accept(folder)
