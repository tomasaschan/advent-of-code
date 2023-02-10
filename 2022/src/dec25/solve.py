#!/usr/bin/env python3

import sys


digits = "=-012"


def dec_to_snafu(decimal):
    snafu = ""

    while decimal > 0:
        decimal += 2
        snafu += digits[(decimal % 5)]
        decimal //= 5

    return "".join(reversed(snafu))


def snafu_to_dec(snafu):
    return sum(
        5**i * (digits.index(x) - 2) for i, x in enumerate(reversed(snafu))
    )


print(
    dec_to_snafu(
        sum(snafu_to_dec(snafu.strip()) for snafu in sys.stdin.readlines())
    )
)
