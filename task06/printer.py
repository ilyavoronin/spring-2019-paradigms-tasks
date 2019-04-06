from model import *


class PrettyPrinter(ASTNodeVisitor):
    def __init__(self):
        self.out = ''

    def visit_number(self, number):
        self.out += str(number.value)

    def visit_function(self, function):
        pass

    def visit_function_definition(self, function_definition):
        pass

    def visit_conditional(self, conditional):
        pass

    def visit_print(self, print_):
        pass

    def visit_read(self, read):
        pass

    def visit_function_call(self, function_call):
        pass

    def visit_reference(self, reference):
        pass

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
