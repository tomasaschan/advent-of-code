package dec17

import (
	"strings"

	"github.com/tomasaschan/advent-of-code-2021/pkg/utils"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/twod"
)

func A(input string) int {
	min, max := parse(input)

	var dy0max int
	for dy0 := min.Y - 3; dy0 <= -min.Y+3; dy0++ {
		hits, _, _ := tBoundsOfY(dy0, min.Y, max.Y)
		if hits {
			dy0max = dy0
		}
	}

	return dy0max * (dy0max + 1) / 2
}

func B(input string) int {
	min, max := parse(input)
	dx0min, dx0max := dxBounds(min.X, max.X)

	velocities := map[twod.Point]bool{}
	for dy0 := min.Y - 3; dy0 <= -min.Y+3; dy0++ {
		hits, t0, t1 := tBoundsOfY(dy0, min.Y, max.Y)
		if !hits {
			continue
		}
		for dx0 := dx0min - 3; dx0 <= dx0max+3; dx0++ {
			for t := t0 - 3; t <= t1+3; t++ {
				xt := x(dx0, t)
				yt := y(dy0, t)
				if min.X <= xt && xt <= max.X && min.Y <= yt && yt <= max.Y {
					velocities[twod.Point{X: dx0, Y: dy0}] = true
				}
			}
		}
	}

	count := 0
	for range velocities {
		count++
	}
	return count
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
