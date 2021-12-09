package dec5

import (
	"strings"

	"github.com/tomasaschan/advent-of-code-2021/pkg/utils"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/twod"
)

func A(input string) int {
	lines := parse(input)
	floor := createMap(lines, false)
	return floor.dangers()
}

func B(input string) int {
	lines := parse(input)
	floor := createMap(lines, true)
	return floor.dangers()
}

type oceanFloor struct {
	m map[twod.Vector]int
}

func (f *oceanFloor) incAt(p twod.Vector) {
	if n, ok := f.m[p]; ok {
		f.m[p] = n + 1
	} else {
		f.m[p] = 1
	}
}

func (f *oceanFloor) mapLine(l twod.Line) {
	for _, p := range l.Points() {
		f.incAt(p)
	}
}

func (f *oceanFloor) dangers() int {
	n := 0
	for _, m := range f.m {
		if m > 1 {
			n++
		}
	}
	return n
}

func parse(input string) []twod.Line {
	lines := make([]twod.Line, 0)

	for _, l := range strings.Split(input, "\n") {
		if l == "" {
			continue
		}

		ints := utils.AllInts(l)
		x1, y1, x2, y2 := ints[0], ints[1], ints[2], ints[3]
		lines = append(lines, twod.Line{Start: twod.Vector{X: x1, Y: y1}, End: twod.Vector{X: x2, Y: y2}})
	}
	return lines
}

func createMap(lines []twod.Line, useDiagonals bool) oceanFloor {
	f := oceanFloor{m: make(map[twod.Vector]int)}

	for _, line := range lines {
		if line.IsDiagonal() && !useDiagonals {
			continue
		}

		f.mapLine(line)
	}

	return f
}
