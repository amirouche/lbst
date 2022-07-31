#!/usr/bin/env python3
import struct
from random import choice
from random import randint
import time

import lbst


INTEGER_MAX = 2**42



kvcount = 2**16


def average(lst):
    return sum(lst) / len(lst)


def timeit(func, *args, **kwargs):
    start = time.perf_counter()
    out = func(*args, **kwargs)
    delta = time.perf_counter() - start
    print(out)
    return delta



def benchmark_lbst(values):
    for _ in range(100):
        out = lbst.make()
        for k, v in values:
            out = lbst.set(out, k, v)

        start = lbst.begin(out)
    return start

# benchmarks

print('benchmarks')
values = []
for kvcount in range(kvcount):
    v = randint(0, INTEGER_MAX)
    values.append((struct.pack('<Q', v), v))

print(kvcount, 'start', len(values))
timing = timeit(benchmark_lbst, values)
print(timing)

# for d, l in zip(ava['dict'], ava['lbst']):
#     ava["speedup"].append(d/l)

# print("Speedup dict vs. lbst, bigger than one is faster")
# for count, speedup in zip(ava['kvcount'], ava['speedup']):
#     print(count, speedup)
