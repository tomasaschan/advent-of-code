#!/usr/bin/env python3

import sys

input = sys.stdin.read()


def parse(input):
    return {
        (x, y)
        for y, line in enumerate(input.splitlines())
        for x, t in enumerate(line.strip())
        if t == "#"
    }


def surroundings(x, y):
    return [
        (x - 1, y - 1),
        (x, y - 1),
        (x + 1, y - 1),
        (x - 1, y),
        (x + 1, y),
        (x - 1, y + 1),
        (x, y + 1),
        (x + 1, y + 1),
    ]


next_to = {
    "N": lambda x, y: (x, y - 1),
    "S": lambda x, y: (x, y + 1),
    "W": lambda x, y: (x - 1, y),
    "E": lambda x, y: (x + 1, y),
}

buffer_zone = {
    "N": lambda x, y: {(x - 1, y - 1), (x, y - 1), (x + 1, y - 1)},
    "S": lambda x, y: {(x - 1, y + 1), (x, y + 1), (x + 1, y + 1)},
    "W": lambda x, y: {(x - 1, y - 1), (x - 1, y), (x - 1, y + 1)},
    "E": lambda x, y: {(x + 1, y - 1), (x + 1, y), (x + 1, y + 1)},
}


def bounds(elves):
    return (
        min(x for x, _ in elves),
        min(y for _, y in elves),
        max(x for x, _ in elves),
        max(y for _, y in elves),
    )


def show(elves):
    minx, miny, maxx, maxy = bounds(elves)
    return "\n".join(
        "".join(
            "#" if (x, y) in elves else "." for x in range(minx - 1, maxx + 2)
        )
        for y in range(miny - 1, maxy + 2)
    )


def free_space(elves):
    minx, miny, maxx, maxy = bounds(elves)

    return (maxx + 1 - minx) * (maxy + 1 - miny) - len(elves)


def round(directions, elves):
    # phase 1: consider moves
    targets = {}
    considered = {}
    for elf in elves:
        if sum(1 for p in surroundings(*elf) if p in elves) > 0:
            for d in directions:
                if any(b in elves for b in buffer_zone[d](*elf)):
                    continue

                target = next_to[d](*elf)
                targets[target] = 1 + targets.get(target, 0)
                considered[elf] = d
                break

    # phase 2: execute moves
    moved = False
    for elf, d in considered.items():
        if targets[next_to[d](*elf)] > 1:
            continue

        moved = True
        elves.remove(elf)
        elves.add(next_to[d](*elf))

    # phase 3: rotate directions
    directions.append(directions.pop(0))
    return moved


def a():
    directions = ["N", "S", "W", "E"]
    elves = parse(input)

    for _ in range(10):
        if not round(directions, elves):
            break

    return free_space(elves)


def b():
    directions = ["N", "S", "W", "E"]
    elves = parse(input)

    i = 1
    while round(directions, elves):
        i += 1

    return i


print("a:", a())
print("b:", b())
