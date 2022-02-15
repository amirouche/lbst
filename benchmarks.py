from random import randint
# from matplotlib.pylab import *
import time

import lbst


MAGIC = 1
INTEGER_MAX = 2**16


# figure(num=None, figsize=(8, 6), dpi=100)

# ax1 = subplot2grid((1, 1), (0, 0))


ava = dict()
ava["kvcount"] = [1_000, 10_000]
ava["list"] = []
ava["dict"] = []
ava["lbst"] = []


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


def benchmark_naive_list(values):
    out = list()
    for value in values:
        out = list(out)
        out.append((value, value))
        out = sorted(out)

    # accumulator = 0
    # for _ in range(MAGIC):
    #     for value in values:
    #         for (k, v) in out:
    #             if k == value:
    #                 some = v
    #                 accumulator += some
    #                 break
    # return accumulator


def benchmark_naive_dict(values):
    out = dict()
    for value in values:
        out = dict(out)
        out[value] = value
        out = sorted(out.items())
        out = dict(out)

    # accumulator = 0
    # for _ in range(MAGIC):
    #     for value in values:
    #         some = out[value]
    #         accumulator += some
    # return accumulator


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
    # ('list', benchmark_naive_list),
    # ('dict', benchmark_naive_dict),
    ('lbst', benchmark_lbst),
)

for kvcount in ava["kvcount"]:
    values = [randint(-INTEGER_MAX, INTEGER_MAX) for _ in range(kvcount)]
    print(kvcount, 'start')
    for name, func in BENCHMARKS:
        timings = []
        for _ in range(MAGIC):
            timing = timeit(func, values)
            timings.append(timing)
        timing = average(timings)
        ava[name].append(timing)
        print(kvcount, name, timing)


# plot(ava['kvcount'], ava['list'], ':', label="list")
# plot(ava['kvcount'], ava['dict'], '-', label="dict")
# plot(ava['kvcount'], ava['lbst'], '^', label="lbst", linewidth=2)
# legend(loc='upper left', fancybox=True, shadow=True, prop=dict(size=10))
# grid(True, which="both", linestyle="dotted")
# ylabel("seconds")
# savefig("benchmarks.png")
