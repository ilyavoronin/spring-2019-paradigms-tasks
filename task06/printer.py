from model import *


class PrettyPrinter(ASTNodeVisitor):
    def __init__(self):
        self.out = ''
        self.indent = 0

    def add_indent(self):
        self.out += '    ' * self.indent

    def add_statements(self, statements):
        self.out += '{\n'
        self.indent += 1
        if statements:
            for stmt in statements:
                self.add_indent()
                stmt.accept(self)
                self.out += ';\n'
        self.indent -= 1
        self.add_indent()
        self.out += '}'

    def visit_number(self, number):
        self.out += str(number.value)

    def visit_function(self, function):
        pass

    def visit_function_definition(self, function_definition):
        self.out += 'def ' + function_definition.name + '('
        self.out += ', '.join(function_definition.function.args) + ') '
        self.add_statements(function_definition.function.body)

    def visit_conditional(self, conditional):
        self.out += 'if ('
        conditional.condition.accept(self)
        self.out += ') '
        self.add_statements(conditional.if_true)
        if conditional.if_false:
            self.out += ' else '
            self.add_statements(conditional.if_false)

    def visit_print(self, print_):
        self.out += 'print '
        print_.expr.accept(self)
        self.out += ';'

    def visit_read(self, read):
        self.out += 'read ' + read.name + ';'

    def visit_function_call(self, function_call):
        pass

    def visit_reference(self, reference):
        self.out += reference.name + ';'

    def visit_binary_operation(self, binary_operation):
        pass

    def visit_unary_operation(self, unary_operation):
        pass

    def print(self):
        print(self.out + ';')


def pretty_print(program):
    printer = PrettyPrinter()
    program.accept(printer)
    printer.print()
