from random import randint
import time

import lbst


MAGIC = 10
INTEGER_MAX = 2**16


ava = dict()
ava["kvcount"] = [1000]
ava["dict"] = []
ava["lbst"] = []
ava["speedup"] = []


def average(lst):
    return sum(lst) / len(lst)


def timeit(func, *args, **kwargs):
    timings = []
    for _ in range(MAGIC):
        start = time.perf_counter()
        func(*args, **kwargs)
        delta = time.perf_counter() - start
        timings.append(delta)
    return average(timings)


def benchmark_naive_dict(values):
    out = dict()
    for value in values:
        out = dict(out)
        out[value] = value
        out = sorted(out.items())
        out = dict(out)

    accumulator = 0
    for _ in range(MAGIC):
        for value in values:
            some = out[value]
            accumulator += some
    return accumulator


def benchmark_lbst(values):
    out = lbst.make()
    for value in values:
        out = lbst.set(out, value, value)

    accumulator = 0
    for _ in range(MAGIC):
        for value in values:
            some = lbst.get(out, value)
            accumulator += some
    return accumulator


BENCHMARKS = (
    ('dict', benchmark_naive_dict),
    ('lbst', benchmark_lbst),
)

# warmup

print('warmup')
for name, func in BENCHMARKS:
    for _ in range(MAGIC):
        values = [randint(-INTEGER_MAX, INTEGER_MAX) for _ in range(100)]
        print(name, timeit(func, values))

# benchmarks

print('benchmarks')
for kvcount in ava["kvcount"]:
    values = [randint(-INTEGER_MAX, INTEGER_MAX) for _ in range(kvcount)]
    print(kvcount, 'start')
    for name, func in BENCHMARKS:
        timings = []
        for _ in range(MAGIC):
            timing = timeit(func, values)
            timings.append(timing)
        timing = average(timings)
        ava[name].append(timing / kvcount)
        print(kvcount, name, timing)

for d, l in zip(ava['dict'], ava['lbst']):
    ava["speedup"].append(d/l)
print(average(ava['speedup']))
