import pytest
from printer import *


def test_visit_number():
    printer = PrettyPrinter()
    num = Number(101)
    num.accept(printer)
    assert printer.out == '101'


def test_visit_conditional():
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


def test_visit_function_definition():
    printer = PrettyPrinter()
    func = Function(['a', 'b'], [Number(12), Number(13)])
    func_def = FunctionDefinition('func', func)
    func_def.accept(printer)
    assert printer.out == \
        'def func(a, b) {\n' + \
        '    12;\n' + \
        '    13;\n' + \
        '}'


def test_visit_print():
    printer = PrettyPrinter()
    print_ = Print(Number(42))
    print_.accept(printer)
    assert printer.out == 'print 42;'


def test_visit_read():
    printer = PrettyPrinter()
    read = Read("aaa")
    read.accept(printer)
    assert printer.out == 'read aaa;'


def test_visit_reference():
    printer = PrettyPrinter()
    ref = Reference('aaa')
    ref.accept(printer)
    assert printer.out == 'aaa;'


def test_unary_operation():
    printer = PrettyPrinter()
    uop = UnaryOperation('-', Number(5))
    uop.accept(printer)
    assert eval(printer.out) == -5


def test_visit_binary_operation():
    printer = PrettyPrinter()
    add1 = BinaryOperation(Number(2), '+', Number(3))
    add2 = BinaryOperation(UnaryOperation('-', Number(4)), '+', Number(5))
    mul = BinaryOperation(add1, '*', add2)
    mul.accept(printer)
    assert eval(printer.out) == 5


if __name__ == "__main__":
    pytest.main(args=['-vv'])
