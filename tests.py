import time
import operator
import random
import sh


def test_mic():
    assert sh._mic()


MAGIC = 100
TREE_MAX_SIZE = random.randint(MAGIC, MAGIC * 100)
INTEGER_MAX = random.randint(MAGIC, MAGIC * 10_000)


def test_balanced_and_sorted_random_trees_of_integers():
    for _ in range(MAGIC):
        # given
        expected = dict()
        tree = sh.make(operator.lt)
        for i in range(TREE_MAX_SIZE):
            key = value = random.randint(-INTEGER_MAX, INTEGER_MAX)
            tree = sh.set(tree, key, value)
            expected[key] = value
        # when
        out = tuple(sh.to_dict(tree).items())
        # then
        assert sh._is_balanced(tree)
        expected = tuple(sorted(expected.items()))
        assert out == expected


def test_balanced_and_sorted_random_trees_of_floats():
    for _ in range(MAGIC):
        # given
        expected = dict()
        tree = sh.make(operator.lt)
        for i in range(TREE_MAX_SIZE):
            key = value = random.uniform(-INTEGER_MAX, INTEGER_MAX)
            tree = sh.set(tree, key, value)
            expected[key] = value
        # when
        out = tuple(sh.to_dict(tree).items())
        # then
        assert sh._is_balanced(tree)
        expected = tuple(sorted(expected.items()))
        assert out == expected


def test_faster_than_naive():
    def make_lbst_tree(values):
        out = sh.make(operator.lt)
        for value in values:
            out = sh.set(out, value, value)
        return out

    def make_naive(values):
        out = dict()
        for value in values:
            out[value] = value
            out = sorted(out.items())
            out = dict(out)
        return out

    values = [random.randint(-INTEGER_MAX, INTEGER_MAX) for _ in range(TREE_MAX_SIZE)]

    start = time.perf_counter()
    make_lbst_tree(values)
    timing_lbst = time.perf_counter() - start

    start = time.perf_counter()
    make_naive(values)
    timing_naive = time.perf_counter() - start

    assert timing_lbst < timing_naive


def test_min():
    for _ in range(MAGIC):
        values = [
            random.randint(-INTEGER_MAX, INTEGER_MAX) for _ in range(TREE_MAX_SIZE)
        ]
        values = sorted(values)

        tree = sh.make(operator.lt)
        for value in values:
            tree = sh.set(tree, value, value)

        assert sh.min(tree) == values[0]


def test_max():
    for _ in range(MAGIC):
        values = [
            random.randint(-INTEGER_MAX, INTEGER_MAX) for _ in range(TREE_MAX_SIZE)
        ]
        values = sorted(values)

        tree = sh.make(operator.lt)
        for value in values:
            tree = sh.set(tree, value, value)

        assert sh.max(tree) == values[-1]


def test_cursor_next():
    for _ in range(MAGIC):
        values = [
            random.randint(-INTEGER_MAX, INTEGER_MAX) for _ in range(TREE_MAX_SIZE)
        ]
        values = sorted(set(values))

        tree = sh.make(operator.lt)
        for value in values:
            tree = sh.set(tree, value, value)

        min = sh.min(tree)
        assert min == values[0]

        cursor = sh.cursor(tree)
        sh.cursor_seek(cursor, min)
        assert sh.cursor_key(cursor) == values[0]

        for index in range(1, len(values)):
            sh.cursor_next(cursor)
            assert sh.cursor_key(cursor) == values[index]

        assert not sh.cursor_next(cursor)


def test_cursor_previous():
    for _ in range(MAGIC):
        values = [
            random.randint(-INTEGER_MAX, INTEGER_MAX) for _ in range(TREE_MAX_SIZE)
        ]
        values = sorted(set(values))

        tree = sh.make(operator.lt)
        for value in values:
            tree = sh.set(tree, value, value)

        max = sh.max(tree)
        assert max == values[len(values) - 1]

        cursor = sh.cursor(tree)
        sh.cursor_seek(cursor, max)
        assert sh.cursor_key(cursor) == values[len(values) - 1]

        for index in range(len(values) - 1, -1):
            sh.cursor_next(cursor)
            assert sh.cursor_key(cursor) == values[index]

        assert not sh.cursor_next(cursor)
