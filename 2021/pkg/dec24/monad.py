def validate(model_number):
    print("validating", model_number)

    print(f"{'j':>3} {'w':>3} {'d':>3} {'xc':>3} {'yc':>3} {'x':>3} {'z':>12} z26")


    def show(z):
        s = ""
        while z > 0:
            s += chr(ord("a") + (z % 26))
            z //= 26
        return "".join(reversed(s))

    w, x, z = 0, 0, 0
    for j, (i, xc, yc, d) in enumerate(
        zip(
            model_number,
            [11, 13, 11, 10, -3, -4, 12, -8, -3, -12, 14, -6, 11, -12],
            [14, 8, 4, 10, 14, 10, 4, 14, 1, 6, 0, 9, 13, 12],
            [1, 1, 1, 1, 26, 26, 1, 26, 26, 26, 1, 26, 1, 26],
        )
    ):
        w = int(i)
        x = 0 if (z % 26) + xc == w else 1
        z = z // d * ((25 * x) + 1) + (w + yc) * x
        print(f"{j:3} {w:3} {d:3} {xc:3} {yc:3} {x:3} {z:>12} {show(z)}")
    print("valid" if z == 0 else "invalid")


# i didn't understand this problem until i read this write-up: https://www.reddit.com/r/adventofcode/comments/rom5l5/2021_day_24pen_paper_monad_deparsed/

# in that notation, my equations end up like this:
#   D0  + 2 = D9
#   D1  + 5 = D8
#   D2      = D5
#   D3  + 7 = D4
#   D6  - 4 = D7
#   D10 - 6 = D11
#   D12 + 1 = D13

# with the following solutions:

smallest = "11118151637112"
largest = "74929995999389"

validate(smallest)
print()
validate(largest)
