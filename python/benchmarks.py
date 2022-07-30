import struct
from random import choice
from random import randint
import time

import lbst


MAGIC = 20
INTEGER_MAX = 2**64


ava = dict()
ava["kvcount"] = [10, 20, 30, 40, 50, 100, 500, 1000]
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
    for k, v in values:
        out = dict(out)
        out[k] = v
        out = sorted(out.items())
        out = dict(out)

    accumulator = 0
    for _ in range(MAGIC):
        for k, _ in values:
            some = out[k]
            accumulator += some
    return accumulator


def benchmark_lbst(values):
    out = lbst.make()
    for k, v in values:
        out = lbst.set(out, k, v)

    accumulator = 0
    for _ in range(MAGIC):
        for k, v in values:
            some = lbst.get(out, k)
            accumulator += some
    return accumulator


BENCHMARKS = (
    ('dict', benchmark_naive_dict),
    ('lbst', benchmark_lbst),
)

print('warmup')
for name, func in BENCHMARKS:
    for _ in range(MAGIC):
        v = randint(0, INTEGER_MAX)
        values = [(struct.pack('<Q', v), v) for _ in range(100)]
        print(choice('_.oO0`'))
        timeit(func, values)

# benchmarks

print('benchmarks')
for kvcount in ava["kvcount"]:
    v = randint(0, INTEGER_MAX)
    values = [(struct.pack('<Q', v), v) for _ in range(kvcount)]
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

print("Speedup dict vs. lbst, bigger than one is faster")
for count, speedup in zip(ava['kvcount'], ava['speedup']):
    print(count, speedup)
