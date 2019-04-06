import pytest
from printer import *


def test_print_number():
    printer = PrettyPrinter()
    num = Number(101)
    num.accept(printer)
    assert printer.out == '101'


if __name__ == "__main__":
    pytest.main()
