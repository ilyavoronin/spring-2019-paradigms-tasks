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


def test_all():
    bi = BinaryOperation(
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
    )
    got = fold_constants(bi)
    assert got == Number(13)


if __name__ == "__main__":
    pytest.main(args=['-vv'])
