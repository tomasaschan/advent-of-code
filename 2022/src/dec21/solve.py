#!/usr/bin/env python3

import sys

input = sys.stdin.readlines()


def parse(lines):
    def parse_line(line):
        key, expr = line.split(":")
        return key, expr.strip()

    return {key: rule for key, rule in map(parse_line, lines)}


def tree(rules, root):
    rule = rules[root]
    if root == "humn":
        return "humn"
    if rule.isnumeric():
        return int(rule)

    expr = rules[root].split()

    return expr[1], tree(rules, expr[0]), tree(rules, expr[2])


def evaluate(tree, **variables):
    if isinstance(tree, int):
        return tree

    if isinstance(tree, str):
        return evaluate(variables[tree])

    assert isinstance(tree, tuple) and len(tree) == 3

    match tree[0]:
        case "+":
            return evaluate(tree[1], **variables) + evaluate(
                tree[2], **variables
            )
        case "-":
            return evaluate(tree[1], **variables) - evaluate(
                tree[2], **variables
            )
        case "*":
            return evaluate(tree[1], **variables) * evaluate(
                tree[2], **variables
            )
        case "/":
            return evaluate(tree[1], **variables) // evaluate(
                tree[2], **variables
            )


def depends_on(tree, variable):
    if isinstance(tree, int):
        return False
    if tree == variable:
        return True

    return depends_on(tree[1], variable) or depends_on(tree[2], variable)


def solve_for(a, b, x):
    assert depends_on(a, x) != depends_on(b, x)
    if depends_on(b, x):
        return solve_for(b, a, x)

    if a == x:
        return b

    assert len(a) == 3

    assert depends_on(a[1], x) != depends_on(a[2], x)

    match a[0], depends_on(a[1], x), depends_on(a[2], x):
        case "+", True, False:
            return solve_for(a[1], ("-", b, a[2]), x)
        case "+", False, True:
            return solve_for(a[2], ("-", b, a[1]), x)
        case "-", True, False:
            return solve_for(a[1], ("+", b, a[2]), x)
        case "-", False, True:
            return solve_for(("-", a[1], b), a[2], x)
        case "*", True, False:
            return solve_for(a[1], ("/", b, a[2]), x)
        case "*", False, True:
            return solve_for(a[2], ("/", b, a[1]), x)
        case "/", True, False:
            return solve_for(a[1], ("*", b, a[2]), x)
        case "/", False, True:
            return solve_for(("/", a[1], b), a[2], x)
        case _:
            raise ValueError


def a():
    rules = parse(input)
    expr = tree(rules, "root")
    return evaluate(expr, humn=int(rules["humn"], 10))


def b():
    rules = parse(input)
    a, b = rules["root"].split()[0], rules["root"].split()[-1]
    a, b = tree(rules, a), tree(rules, b)

    return evaluate(solve_for(a, b, "humn"))


print("a:", a())
print("b:", b())
