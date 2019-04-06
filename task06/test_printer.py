import pytest
from printer import *


def test_print_number():
    printer = PrettyPrinter()
    num = Number(101)
    num.accept(printer)
    assert printer.out == '101'


def test_print_conditional():
    printer = PrettyPrinter()
    cond1 = Conditional(Number(1), [Number(10), Number(11)])
    cond2 = Conditional(Number(42), [cond1, Number(12)], [Number(13)])
    cond2.accept(printer)
    assert printer.out == \
        'if (42) {\n' + \
        '    if (1) {\n' + \
        '        10;\n' + \
        '        11;\n' + \
        '    };\n' + \
        '    12;\n' + \
        '} else {\n' + \
        '    13;\n' + \
        '}'


def test_function_definition():
    printer = PrettyPrinter()
    func = Function(['a', 'b'], [Number(12), Number(13)])
    func_def = FunctionDefinition('func', func)
    func_def.accept(printer)
    assert printer.out == \
        'def func(a, b) {\n' + \
        '    12;\n' + \
        '    13;\n' + \
        '}'


if __name__ == "__main__":
    pytest.main(args=['-vv'])
