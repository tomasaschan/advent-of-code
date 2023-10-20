#!/usr/bin/env python3

import concurrent.futures
import contextlib
import math
import sys
import time


RESOURCE_TYPES = {
    t: i for i, t in enumerate(("ore", "clay", "obsidian", "geode"))
}
VERBOSE = False


def parse(lines):
    def costs(recipie):
        robot = recipie.removeprefix("Each ")
        robot, costs = robot.split(" ", 1)
        lookup = {
            cost.split(" ")[1]: int(cost.split(" ")[0])
            for cost in costs.removeprefix("robot costs ").split(" and ")
        }

        return robot, tuple(lookup.get(t, 0) for t in RESOURCE_TYPES)

    def recipies(robots):
        for recipie in robots.removesuffix(".").split(". "):
            yield costs(recipie)

    for line in lines:
        name, robots = line.strip().split(": ", 1)
        k = int(name.removeprefix("Blueprint "))

        yield k, {t: c for t, c in recipies(robots)}


blueprints = list(parse(sys.stdin.readlines()))


D = 0


class State:
    def __init__(
        self,
        time_left,
        resources=None,
        robots=None,
        max_robots=None,
        blueprint=None,
        path=None,
    ):
        self.time_left = time_left

        self.resources = resources or (0, 0, 0, 0)
        self.robots = robots or (1, 0, 0, 0)
        if not max_robots and blueprint:
            self.max_robots = tuple(
                max(
                    blueprint[robot][RESOURCE_TYPES[t]]
                    for robot in RESOURCE_TYPES
                )
                for t in RESOURCE_TYPES
            )
        elif max_robots:
            self.max_robots = max_robots
        else:
            raise ValueError("must provide either max_robots or blueprint")

        self.path = path or []

    def harvest(self, t):
        return tuple(r + t * b for r, b in zip(self.resources, self.robots))

    def buy(self, robot, blueprint):
        time_required = max(
            (
                math.ceil((c - r) / b)
                for c, r, b in zip(
                    blueprint[robot], self.resources, self.robots
                )
                if c > 0 and c - r > 0
            ),
            default=0,
        )

        if time_required >= self.time_left:
            return self.wait()

        resources = tuple(
            r - c
            for r, c in zip(self.harvest(time_required + 1), blueprint[robot])
        )
        robots = tuple(
            b + 1 if i == RESOURCE_TYPES[robot] else b
            for i, b in enumerate(self.robots)
        )

        return State(
            self.time_left - time_required - 1,
            resources,
            robots,
            self.max_robots,
            path=[*self.path, (self.time_left - time_required, robot)],
        )

    def wait(self):
        return State(
            0,
            self.harvest(self.time_left),
            self.robots,
            max_robots=self.max_robots,
            path=[*self.path, (0, "wait")],
        )

    def nexts(self, blueprint):
        def produce(robot):
            if robot != "geode" and (
                self.robots[RESOURCE_TYPES[robot]]
                >= self.max_robots[RESOURCE_TYPES[robot]]
            ):
                # ignore robots we don't need more of
                return None

            cost = blueprint[robot]

            if any(
                c > r and b == 0
                for r, b, c in zip(self.resources, self.robots, cost)
            ):
                # ignore robots we won't be able to afford
                # unless we build something else first
                return None

            return self.buy(robot, blueprint)

        yield from (
            r
            for r in (produce(robot) for robot in RESOURCE_TYPES)
            if r is not None
        )
        yield self.wait()

    def theoretical_best(self):
        return (
            # already opened geodes
            self.resources[RESOURCE_TYPES["geode"]]
            +
            # geodes to be opened by already owned robots
            self.robots[RESOURCE_TYPES["geode"]] * self.time_left
            +
            # geodes opened by creating a new geode bot every minute
            self.time_left * (self.time_left - 1) // 2
        )


def max_geodes(blueprint, time_limit):
    q = [State(time_limit, 0, blueprint=blueprint)]
    best = 0

    while q:
        s = q.pop()

        if s.theoretical_best() < best:
            continue

        if s.time_left == 0:
            best = max(best, s.resources[RESOURCE_TYPES["geode"]])
            continue

        for n in s.nexts(blueprint):
            q.append(n)

    return best


@contextlib.contextmanager
def measured(label):
    start = time.perf_counter()
    yield
    end = time.perf_counter()
    print(label, "done in", f"{end-start:5.2f} s")


def parallel_run(blueprints, time_limit):
    def run(i, b):
        return i, max_geodes(b, time_limit)

    with concurrent.futures.ThreadPoolExecutor() as x:
        return x.map(run, *zip(*blueprints))


def a():
    return sum(i * g for i, g in parallel_run(blueprints, 24))


def b():
    return math.prod(g for _, g in parallel_run(blueprints[:3], 32))


if __name__ == "__main__":
    with measured("19"):
        print("a:", a())
        print("b:", b())
