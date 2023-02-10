#!/usr/bin/env python3

import re
import sys

terrain_input, instructions_input = sys.stdin.read().split("\n\n", 1)
dirs = [">", "v", "<", "^"]
turns = ["L", "R"]

terrain = {
    (x + 1, y + 1): t == "."
    for y, line in enumerate(terrain_input.splitlines())
    for x, t in enumerate(line)
    if t != " "
}

rx = re.compile(r"\d+|[RL]")
instructions = [
    x if x in {"R", "L"} else int(x, 10)
    for x in rx.findall(instructions_input)
]


def start():
    return (min(x for x, y in terrain.keys() if y == 1), 1)


def turn_rigth(d):
    return dirs[(dirs.index(d) + 1) % len(dirs)]


def turn_left(d):
    return dirs[(dirs.index(d) - 1) % len(dirs)]


turn = {"L": turn_left, "R": turn_rigth}


def password(d, x, y):
    return 1000 * y + 4 * x + dirs.index(d)


def solve(next_to):
    d, x = ">", start()

    for i in instructions:
        if isinstance(i, int):
            for _ in range(i):
                nxt = next_to[d](*x)
                if terrain[nxt[0]]:
                    x, d = nxt
                else:
                    break
        else:
            d = turn[i](d)

    return password(d, *x)


def a():
    def right_of(x, y):
        if (x + 1, y) in terrain:
            return (x + 1, y), ">"
        else:
            return (min(x for x, j in terrain.keys() if j == y), y), ">"

    def left_of(x, y):
        if (x - 1, y) in terrain:
            return (x - 1, y), "<"
        else:
            return (max(x for x, j in terrain.keys() if j == y), y), "<"

    def above(x, y):
        if (x, y - 1) in terrain:
            return (x, y - 1), "^"
        else:
            return (x, max(y for i, y in terrain.keys() if i == x)), "^"

    def below(x, y):
        if (x, y + 1) in terrain:
            return (x, y + 1), "v"
        else:
            return (x, min(y for i, y in terrain.keys() if i == x)), "v"

    return solve({">": right_of, "v": below, "<": left_of, "^": above})


def b():
    # assuming input on the form
    #
    #  AB
    #  C
    # DE
    # F

    def right_of(x, y):
        if (x + 1, y) in terrain:
            return (x + 1, y), ">"
        if y <= 50:
            # moving from right of B to right of E
            return (100, 151 - y), "<"
        if y <= 100:
            # moving from right of C to bottom of B
            return (y + 50, 50), "^"
        if y <= 150:
            # moving from right of E to right of B
            return (150, 151 - y), "<"
        if y <= 200:
            # moving from right of F to bottom of E
            return (y - 100, 150), "^"
        raise NotImplementedError(f"can't move right from ({x}, {y})")

    def left_of(x, y):
        if (x - 1, y) in terrain:
            return (x - 1, y), "<"
        if y <= 50:
            # moving from left of A to left of D
            return (1, 151 - y), ">"
        if y <= 100:
            # moving from left of C to top of D
            return (y - 50, 101), "v"
        if y <= 150:
            # moving from left of D to left of A
            return (51, 151 - y), ">"
        if y <= 200:
            # moving from left of F to top of A
            return (y - 100, 1), "v"
        raise NotImplementedError(f"can't move left from ({x}, {y})")

    def above(x, y):
        if (x, y - 1) in terrain:
            return (x, y - 1), "^"
        if x <= 50:
            # moving from top of D to left of C
            return (51, 50 + x), ">"
        if x <= 100:
            # moving from top of A to left of F
            return (1, 100 + x), ">"
        if x <= 150:
            # moving from top of B to bottom of F
            return (x - 100, 200), "^"
        raise NotImplementedError(f"can't move up from ({x}, {y})")

    def below(x, y):
        if (x, y + 1) in terrain:
            return (x, y + 1), "v"
        if x <= 50:
            # moving from bottom of F to top of B
            return (x + 100, 1), "v"
        if x <= 100:
            # moving from bottom of E to right of F
            return (50, 100 + x), "<"
        if x <= 150:
            # moving from bottom of B to right of C
            return (100, x - 50), "<"
        raise NotImplementedError(f"can't move up from ({x}, {y})")

    return solve({">": right_of, "v": below, "<": left_of, "^": above})


print("a:", a())
print("b:", b())
