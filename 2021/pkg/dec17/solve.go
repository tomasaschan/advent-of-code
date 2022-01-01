package dec17

import (
	"strings"

	"github.com/tomasaschan/advent-of-code-2021/pkg/utils"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/twod"
)

func A(input string) int {
	min, max := parse(input)

	dy0s := hitsY(min.Y, max.Y)
	dy0max := dy0s[len(dy0s)-1]

	return dy0max * (dy0max + 1) / 2
}

func B(input string) int {
	min, max := parse(input)

	dx0s := hitsX(min.X, max.X)
	dy0s := hitsY(min.Y, max.Y)

	count := 0
	for _, dx0 := range dx0s {
		for _, dy0 := range dy0s {
			t := 0
			xp := -1
			for {
				xt := x(dx0, t)
				yt := y(dy0, t)
				if xp == xt && xt < min.X {
					// will never reach target
					break
				} else if max.X < xt || yt < min.Y {
					// missed
					break
				} else if min.X <= xt && xt <= max.X && min.Y <= yt && yt <= max.Y {
					// hit!
					count++
					break
				}
				xp = xt
				t++
			}
		}
	}

	return count
}

func x(dx0, t int) int {
	// total x travelled can be divided into two parts:
	// 1. as if travelling with constant speed dx0, i.e. dx0*t
	// 2. a correction term from drag,
	//    starting at 0 and growing by t each step until t=dx0,
	//	  then growing by dx0 each step

	x := dx0 * t

	if t < dx0 {
		x -= t * (t - 1) / 2
	} else {
		x -= dx0 * (dx0 - 1) / 2
		x -= (t - dx0) * dx0
	}
	return x
}

func y(dy0, t int) int {
	// total y travelled can be divided into two parts:
	// 1. as if travelling with constant speed dy0, i.e. dy0*t
	// 2. a correction term from gravity, starting at 0 and growing by t each step
	// the latter is an arithmetic sum of t terms, starting at 0 and ending at t-1
	y := dy0*t - t*(t-1)/2

	return y
}

func hitsX(min int, max int) []int {
	dx0s := make([]int, 0)

	for dx0 := 0; dx0 <= max; dx0++ {
		t := 0
		xp := -1
		for {
			xt := x(dx0, t)
			if xp == xt && xt < min {
				// will never reach target
				break
			} else if min <= xt && xt <= max {
				dx0s = append(dx0s, dx0)
				break
			} else if max < xt {
				// missed
				break
			}
			t++
			xp = xt
		}
	}
	return dx0s
}

func hitsY(min int, max int) []int {
	dy0s := make([]int, 0)
	for dy0 := min; dy0 <= -min; dy0++ {
		t := 0
		for {
			yt := y(dy0, t)
			if yt < min {
				break
			} else if yt <= max {
				dy0s = append(dy0s, dy0)
				break
			}
			t++
		}
	}
	return dy0s
}

func parse(input string) (twod.Point, twod.Point) {
	boundaries := utils.AllInts(strings.TrimSpace(input))
	var xmin, ymin, xmax, ymax int
	if boundaries[0] < boundaries[1] {
		xmin, xmax = boundaries[0], boundaries[1]
	} else {
		xmin, xmax = boundaries[1], boundaries[0]
	}
	if boundaries[2] < boundaries[3] {
		ymin, ymax = boundaries[2], boundaries[3]
	} else {
		ymin, ymax = boundaries[3], boundaries[2]
	}

	if xmin <= 0 || ymax >= 0 {
		panic("this solution only works if the target area is to the right and below...")
	}

	return twod.Point{X: xmin, Y: ymin}, twod.Point{X: xmax, Y: ymax}
}
