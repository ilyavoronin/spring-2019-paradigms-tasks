import pytest
from folder import *
from printer import *


def test_num_x_num():
    folder = ConstantFolder()
    bi = BinaryOperation(Number(10), '*', Number(11))
    got = bi.accept(folder)
    assert got == Number(110)


def test_x_num():
    folder = ConstantFolder()
    un = UnaryOperation('-', Number(10))
    got = un.accept(folder)
    assert got == Number(-10)


def test_ref_x_num():
    folder = ConstantFolder()
    bi = BinaryOperation(Reference('a'), '*', Number(0))
    got = bi.accept(folder)
    assert got == Number(0)


def test_num_x_ref():
    folder = ConstantFolder()
    bi = BinaryOperation(Number(0), '*', Reference('a'))
    got = bi.accept(folder)
    assert got == Number(0)


def test_ref_x_ref():
    folder = ConstantFolder()
    bi = BinaryOperation(Reference('a'), '-', Reference('a'))
    got = bi.accept(folder)
    assert got == Number(0)


def test_conditional_none():
    folder = ConstantFolder()
    cond = Conditional(Number(1), None, None)
    got = cond.accept(folder)
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
    assert got.condition == Number(13) and got.if_true == [
        Number(0)] and got.if_false == [Number(-10)]


if __name__ == "__main__":
    pytest.main(args=['-vv'])
