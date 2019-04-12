import pytest
from printer import *
import textwrap


def test_visit_number():
    printer = PrettyPrinter()
    num = Number(101)
    num.accept(printer)
    assert printer.get_out() == '101;\n'


def test_visit_conditional():
    printer = PrettyPrinter()
    cond1 = Conditional(Number(1), [Number(10), Number(11)])
    cond2 = Conditional(Number(42), [cond1, Number(12)], [Number(13)])
    cond2.accept(printer)
    assert printer.get_out() == textwrap.dedent(
        '''\
        if (42) {
            if (1) {
                10;
                11;
            }
            12;
        } else {
            13;
        }
        '''
    )


def test_visit_function_definition():
    printer = PrettyPrinter()
    func = Function(['a', 'b'], [Number(12), Number(13)])
    func_def = FunctionDefinition('func', func)
    func_def.accept(printer)
    assert printer.get_out() == textwrap.dedent(
        '''\
        def func(a, b) {
            12;
            13;
        }
        '''
    )


def test_visit_print():
    printer = PrettyPrinter()
    print_ = Print(Number(42))
    print_.accept(printer)
    assert printer.get_out() == 'print 42;\n'


def test_visit_read():
    printer = PrettyPrinter()
    read = Read("aaa")
    read.accept(printer)
    assert printer.get_out() == 'read aaa;\n'


def test_visit_reference():
    printer = PrettyPrinter()
    ref = Reference('aaa')
    ref.accept(printer)
    assert printer.get_out() == 'aaa;\n'


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


def test_function_call():
    printer = PrettyPrinter()
    func_call1 = FunctionCall(Reference('f1'), [Number(10), Number(11)])
    func_call2 = FunctionCall(Reference('f2'), [Number(12), func_call1])
    func_call2.accept(printer)
    assert printer.get_out() == 'f2(12, f1(10, 11));\n'


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
    print(out)
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


if __name__ == "__main__":
    pytest.main(args=['-vv'])
