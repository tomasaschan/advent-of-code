#!/usr/bin/env python3

import collections
import sys

input = sys.stdin.read()


def parse(
    input: str,
) -> tuple[
    tuple[int, int, int, int],
    dict[tuple[int, int], list[str]],
    tuple[int, int],
    tuple[int, int],
]:
    valley = {
        (x, y): c
        for y, line in enumerate(input.splitlines())
        for x, c in enumerate(line.strip())
    }
    bounds = (
        min(x for x, _ in valley),
        min(y for _, y in valley),
        max(x for x, _ in valley),
        max(y for _, y in valley),
    )
    blizzards: dict[tuple[int, int], list[str]] = {}
    for p, c in valley.items():
        if c in {"<", ">", "^", "v"}:
            blizzards[p] = [c] if p not in blizzards else [*blizzards[p], c]
    start = next((x, y) for (x, y), c in valley.items() if y == 0 and c != "#")
    goal = next(
        (x, y)
        for (x, y), c in valley.items()
        if y == max(y for _, y in valley)
        if c != "#"
    )

    return bounds, blizzards, start, goal


def show(bounds, blizzards, pos):
    minx, miny, maxx, maxy = bounds

    simplified_blizzards = {}
    for p, c in blizzards:
        if p not in simplified_blizzards:
            simplified_blizzards[p] = c
        elif isinstance(simplified_blizzards[p], str):
            simplified_blizzards[p] = 2
        else:
            simplified_blizzards[p] += 1

    return "\n".join(
        "".join(
            "E"
            if (x, y) == pos
            else "#"
            if x in {minx, maxx} or y in {miny, maxy}
            else str(simplified_blizzards.get((x, y), "."))
            for x in range(minx, maxx + 1)
        )
        for y in range(miny, maxy + 1)
    )


def move_blizzards(bounds, blizzards):
    minx, miny, maxx, maxy = bounds

    def move(x, y, c):
        match c:
            case "<":
                return (x - 1 if x - 1 > minx else maxx - 1, y), c
            case ">":
                return (x + 1 if x + 1 < maxx else minx + 1, y), c
            case "^":
                return (x, y - 1 if y - 1 > miny else maxy - 1), c
            case "v":
                return (x, y + 1 if y + 1 < maxy else miny + 1), c
            case _:
                raise NotImplementedError()

    result = {}
    for p, c in (
        move(x, y, c) for (x, y), cs in blizzards.items() for c in cs
    ):
        result[p] = [c] if p not in result else [*result[p], c]

    return result


def hike(bounds, blizzards, start, goal):
    minx, miny, maxx, maxy = bounds
    N = -1

    q: collections.deque[
        tuple[
            tuple[int, int],
            int,
            list[tuple[int, int]],
        ]
    ] = collections.deque([(start, 0, [start])])

    seen = set()

    while q:
        (x, y), n, path = q.popleft()

        if n != N:
            blizzards = move_blizzards(bounds, blizzards)
            N = n

        for (u, v) in ((x, y), (x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)):
            if (u, v) == goal:
                return n + 1, blizzards
            if (u, v) in blizzards:
                continue
            if (u, v) != start and (
                u <= minx or u >= maxx or v <= miny or v >= maxy
            ):
                continue

            if (u, v, n + 1) in seen:
                continue

            seen.add((u, v, n + 1))
            q.append(((u, v), n + 1, [*path, (u, v)]))

    raise Exception("did not reach goal!")


def a():
    bounds, blizzards, start, goal = parse(input)
    n, _ = hike(bounds, blizzards, start, goal)
    return n


def b():
    bounds, blizzards, start, goal = parse(input)

    i, blizzards = hike(bounds, blizzards, start, goal)
    j, blizzards = hike(bounds, blizzards, goal, start)
    k, _ = hike(bounds, blizzards, start, goal)

    return i + j + k


print("a:", a())
print("b:", b())
