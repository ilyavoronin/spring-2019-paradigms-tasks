from model import *


class PrettyPrinter(ASTNodeVisitor):
    def __init__(self):
        self.indent = 0

    def get_indent(self):
        return '    ' * self.indent

    def visit_block(self, statements):
        res = '{\n'
        self.indent += 1
        for stmt in statements or []:
            res += self.get_indent() + stmt.accept(self)
            if res[-1] != '}':
                res += ';'
            res += '\n'
        self.indent -= 1
        res += self.get_indent() + '}'
        return res

    def visit_number(self, number):
        return str(number.value)

    def visit_function(self, function):
        raise TypeError("Can't print Function object")

    def visit_function_definition(self, function_definition):
        return ('def ' + function_definition.name + '(' +
                ', '.join(function_definition.function.args) + ') ' +
                self.visit_block(function_definition.function.body))

    def visit_conditional(self, conditional):
        res = 'if (' + conditional.condition.accept(self) + ') '
        res += self.visit_block(conditional.if_true)
        if conditional.if_false:
            res += ' else ' + self.visit_block(conditional.if_false)
        return res

    def visit_print(self, print_):
        return 'print ' + print_.expr.accept(self)

    def visit_read(self, read):
        return 'read ' + read.name

    def visit_function_call(self, function_call):
        res = function_call.fun_expr.accept(self) + '('
        for i, arg in enumerate(function_call.args):
            if i:
                res += ', '
            res += arg.accept(self)
        return res + ')'

    def visit_reference(self, reference):
        return reference.name

    def visit_binary_operation(self, binary_operation):
        return ('(' + binary_operation.lhs.accept(self) + ') ' +
                binary_operation.op + ' (' +
                binary_operation.rhs.accept(self) + ')')

    def visit_unary_operation(self, unary_operation):
        return (unary_operation.op + '(' +
                unary_operation.expr.accept(self) + ')')


def pretty_print(program):
    printer = PrettyPrinter()
    res = program.accept(printer)
    if res[-1] != '}':
        res += ';'
    print(res)
