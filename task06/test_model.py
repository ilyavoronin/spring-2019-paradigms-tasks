#!/usr/bin/env python3
import pytest
from model import *
from io import StringIO
import sys


def test_scope_cur():
    s = Scope()
    s['a'] = 5
    s['b'] = 'sdsds'

    def f():
        pass

    s['c'] = f
    assert s['a'] == 5
    assert s['b'] == 'sdsds'
    assert s['c'] == f


def test_scope_grandparent():
    s1 = Scope()
    s2 = Scope(s1)
    s3 = Scope(s2)
    s1['a'] = 1
    s1['b'] = 2
    s1['c'] = 3
    s2['a'] = 4
    s3['b'] = 5
    assert s3['a'] == 4
    assert s3['b'] == 5
    assert s3['c'] == 3


def test_scope_error():
    s1 = Scope()
    s2 = Scope(s1)
    s3 = Scope(s2)
    with pytest.raises(KeyError):
        a = s3['a']


def test_function_definition():
    def f():
        pass

    func_def = FunctionDefinition('f', f)
    scope = Scope()
    ret_func = func_def.evaluate(scope)
    assert scope['f'] == f
    assert ret_func == f


def test_conditional_false():
    cond = Conditional(Number(0), [Number(1)], [Number(2), Number(3)])
    scope = Scope()
    assert cond.evaluate(scope) == Number(3)


def test_conditional_true():
    cond = Conditional(Number(5), [Number(1), Number(4)], [Number(2)])
    scope = Scope()
    assert cond.evaluate(scope) == Number(4)


def test_print(capsys):
    pr = Print(Number(65))
    scope = Scope()
    assert pr.evaluate(scope) == Number(65)
    assert capsys.readouterr().out == '65\n'


def test_read(monkeypatch):
    r = Read('a')
    scope = Scope()
    monkeypatch.setattr(sys, 'stdin', StringIO('66'))
    assert r.evaluate(scope) == Number(66)
    assert scope['a'] == Number(66)


def test_function_call():
    scope = Scope()
    FunctionDefinition('func', Function(['a', 'b'],
                       [BinaryOperation(Reference('a'), '*',
                                        Reference('b'))])).evaluate(scope)
    func_call = FunctionCall(Reference('func'), [Number(5), Number(6)])
    assert func_call.evaluate(scope) == Number(30)


def test_reference():
    ref = Reference('abc')
    scope = Scope()
    scope['abc'] = Number(834)
    assert ref.evaluate(scope) == Number(834)


def test_binary_operation():
    scope = Scope()
    assert BinaryOperation(Number(3), '+', Number(5)
                           ).evaluate(scope) == Number(8)
    assert BinaryOperation(Number(6), '-', Number(2)
                           ).evaluate(scope) == Number(4)
    assert BinaryOperation(Number(3), '*', Number(4)
                           ).evaluate(scope) == Number(12)
    assert BinaryOperation(Number(5), '/', Number(2)
                           ).evaluate(scope) == Number(2)
    assert BinaryOperation(Number(10), '%', Number(4)
                           ).evaluate(scope) == Number(2)
    assert BinaryOperation(Number(5), '==', Number(5)
                           ).evaluate(scope) != Number(0)
    assert BinaryOperation(Number(5), '!=', Number(5)
                           ).evaluate(scope) == Number(0)
    assert BinaryOperation(Number(3), '<', Number(5)
                           ).evaluate(scope) != Number(0)
    assert BinaryOperation(Number(5), '>', Number(7)
                           ).evaluate(scope) == Number(0)
    assert BinaryOperation(Number(5), '<=', Number(5)
                           ).evaluate(scope) != Number(0)
    assert BinaryOperation(Number(5), '>=', Number(5)
                           ).evaluate(scope) != Number(0)
    assert BinaryOperation(Number(2), '&&', Number(1)
                           ).evaluate(scope) != Number(0)
    assert BinaryOperation(Number(1), '||', Number(0)
                           ).evaluate(scope) == Number(1)


def test_unary_operator():
    scope = Scope()
    assert UnaryOperation('!', Number(100)).evaluate(scope) == Number(0)
    assert UnaryOperation('-', Number(7)).evaluate(scope) == Number(-7)


def test_all():
    fac = FunctionDefinition('fac', Function(['n'], [
        Conditional(
            BinaryOperation(Reference('n'), '==', Number(0)),
            [Number(1)],
            [
                BinaryOperation(
                    Reference('n'),
                    '*',
                    FunctionCall(Reference('fac'), [
                        BinaryOperation(
                            Reference('n'),
                            '-',
                            Number(1)
                        )
                    ])
                )
            ]
        )
    ]))
    scope = Scope()
    func_call = FunctionCall(fac, [Number(7)])
    assert func_call.evaluate(scope) == Number(5040)


if __name__ == "__main__":
    pytest.main()
