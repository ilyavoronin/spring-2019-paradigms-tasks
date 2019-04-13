import pytest
from printer import *
import textwrap
import sys


def test_visit_number():
    num = Number(101)
    assert num.accept(PrettyPrinter()) == '101'


def test_visit_conditional():
    cond1 = Conditional(Number(1), [Number(10), Number(11)])
    cond2 = Conditional(Number(42), [cond1, Number(12)], [Number(13)])
    assert cond2.accept(PrettyPrinter()) == textwrap.dedent(
        '''\
        if (42) {
            if (1) {
                10;
                11;
            }
            12;
        } else {
            13;
        }'''
    )


def test_visit_function_definition():
    func = Function(['a', 'b'], [Number(12), Number(13)])
    func_def = FunctionDefinition('func', func)
    assert func_def.accept(PrettyPrinter()) == textwrap.dedent(
        '''\
        def func(a, b) {
            12;
            13;
        }'''
    )


def test_visit_print():
    print_ = Print(Number(42))
    assert print_.accept(PrettyPrinter()) == 'print 42'


def test_visit_read():
    read = Read('aaa')
    assert read.accept(PrettyPrinter()) == 'read aaa'


def test_visit_reference():
    ref = Reference('aaa')
    assert ref.accept(PrettyPrinter()) == 'aaa'


def test_unary_operation():
    uop = UnaryOperation('-', Number(5))
    assert uop.accept(PrettyPrinter()) == '-(5)'


def test_unary_operation2():
    uop = UnaryOperation('-', UnaryOperation('-', Reference('a')))
    assert uop.accept(PrettyPrinter()) == '-(-(a))'


def test_visit_binary_operation():
    add1 = BinaryOperation(Number(2), '+', Number(3))
    add2 = BinaryOperation(UnaryOperation('-', Number(4)), '+', Number(5))
    mul = BinaryOperation(add1, '*', add2)
    assert mul.accept(PrettyPrinter()) == '((2) + (3)) * ((-(4)) + (5))'


def test_function_call():
    func_call1 = FunctionCall(Reference('f1'), [Number(10), Number(11)])
    func_call2 = FunctionCall(Reference('f2'), [Number(12), func_call1])
    assert func_call2.accept(PrettyPrinter()) == 'f2(12, f1(10, 11))'


def test_all(capsys):
    pretty_print(FunctionDefinition('main', Function(['arg1'], [
        Read('x'),
        Print(Reference('x')),
        Conditional(
            BinaryOperation(Number(2), '==', Number(3)),
            [
                Conditional(Number(1), [], [])
            ],
            [
                FunctionCall(Reference('exit'), [
                    UnaryOperation('-', Reference('arg1'))
                ])
            ],
        ),
    ])))
    out = capsys.readouterr().out
    assert out == textwrap.dedent(
        '''\
        def main(arg1) {
            read x;
            print x;
            if ((2) == (3)) {
                if (1) {
                }
            } else {
                exit(-(arg1));
            }
        }
        '''
    )


if __name__ == '__main__':
    pytest.main(sys.argv)
