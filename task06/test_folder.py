import pytest
from folder import *
from printer import *
import sys


def test_mult_num_num():
    bi = BinaryOperation(Number(10), '*', Number(11))
    got = bi.accept(ConstantFolder())
    assert got == Number(110)


def test_unary_minus_num():
    un = UnaryOperation('-', Number(10))
    got = un.accept(ConstantFolder())
    assert got == Number(-10)


def test_mult_ref_num0():
    bi = BinaryOperation(Reference('a'), '*', Number(0))
    got = bi.accept(ConstantFolder())
    assert got == Number(0)


def test_mult_num0_ref():
    bi = BinaryOperation(Number(0), '*', Reference('a'))
    got = bi.accept(ConstantFolder())
    assert got == Number(0)


def test_subtraction_ref_ref():
    bi = BinaryOperation(Reference('a'), '-', Reference('a'))
    got = bi.accept(ConstantFolder())
    assert got == Number(0)


def test_conditional_none():
    cond = Conditional(Number(1), None, None)
    got = cond.accept(ConstantFolder())
    return got == cond


def test_all():
    bi = Conditional(
        BinaryOperation(
            Number(10),
            '-',
            UnaryOperation(
                '-',
                BinaryOperation(
                    Number(3),
                    '+',
                    BinaryOperation(
                        Reference('x'),
                        '-',
                        Reference('x')
                    )
                )
            )
        ), [BinaryOperation(Number(0), '*',  Reference('dsdsd'))],
        [UnaryOperation('-', Number(10))]
    )
    got = fold_constants(bi)
    assert (isinstance(got, Conditional) and got.condition == Number(13) and
            got.if_true == [Number(0)] and got.if_false == [Number(-10)])


if __name__ == '__main__':
    pytest.main(sys.argv)
