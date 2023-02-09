#!/usr/bin/env python3

import sys

initial = [int(line.strip()) for line in sys.stdin.readlines()]


def mix1(data, k, i):
    x = data.pop(i)
    j = (i + x[1]) % len(data)
    data.insert(j, x)


def mix(data):
    i = [j for j, _ in data].index(0)
    for k in range(len(data)):
        while data[i][0] != k:
            i += 1
            i %= len(data)
        mix1(data, k, i)


def coordinates(data: list[int]):
    root = data.index(0)

    return (
        data[(root + 1000) % len(data)],
        data[(root + 2000) % len(data)],
        data[(root + 3000) % len(data)],
    )


def a():
    data = list(enumerate(initial))
    mix(data)
    result = [x for _, x in data]

    return sum(coordinates(result))


def b():
    key = 811589153

    data = list(enumerate(x * key for x in initial))
    for _ in range(10):
        mix(data)
    result = [x for _, x in data]

    return sum(coordinates(result))


print("a:", a())
print("b:", b())
