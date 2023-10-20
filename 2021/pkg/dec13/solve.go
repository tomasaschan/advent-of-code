package dec13

import (
	"math"
	"strconv"
	"strings"

	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/twod"
)

func A(input string) int {
	paper, folds := parse(input)

	folded := fold(paper, folds[0])

	return len(folded)
}

func B(input string) string {
	paper, folds := parse(input)
	xmax, ymax := math.MaxInt, math.MaxInt

	for _, f := range folds {
		paper = fold(paper, f)
		if f > 0 {
			xmax = f
		} else {
			ymax = -f
		}
	}

	sb := strings.Builder{}

	for y := 0; y < ymax; y++ {
		for x := 0; x < xmax; x++ {
			if _, ok := paper[twod.Vector{X: x, Y: y}]; ok {
				sb.WriteString("#")
			} else {
				sb.WriteString(".")
			}
		}
		sb.WriteString("\n")
	}

	return sb.String()
}

func fold(paper map[twod.Vector]bool, coord int) map[twod.Vector]bool {
	if coord > 0 {
		return foldAlongX(paper, coord)
	} else {
		return foldAlongY(paper, -coord)
	}
}

func foldAlongY(paper map[twod.Vector]bool, coord int) map[twod.Vector]bool {
	folded := map[twod.Vector]bool{}

	for dot := range paper {
		if dot.Y < coord {
			folded[dot] = true
		} else {
			folded[twod.Vector{X: dot.X, Y: coord - (dot.Y - coord)}] = true
		}
	}

	return folded
}
func foldAlongX(paper map[twod.Vector]bool, coord int) map[twod.Vector]bool {
	folded := map[twod.Vector]bool{}

	for dot := range paper {
		if dot.X < coord {
			folded[dot] = true
		} else {
			folded[twod.Vector{X: coord - (dot.X - coord), Y: dot.Y}] = true
		}
	}

	return folded
}

func parse(input string) (map[twod.Vector]bool, []int) {
	parts := strings.Split(input, "\n\n")

	paper := map[twod.Vector]bool{}
	folds := make([]int, 0)

	for _, line := range strings.Split(parts[0], "\n") {
		coords := strings.Split(line, ",")
		x, _ := strconv.Atoi(coords[0])
		y, _ := strconv.Atoi(coords[1])
		paper[twod.Vector{X: x, Y: y}] = true
	}

	for _, line := range strings.Split(parts[1], "\n") {
		if line == "" {
			continue
		}
		line = strings.TrimPrefix(line, "fold along ")
		fold := strings.Split(line, "=")
		coord, _ := strconv.Atoi(fold[1])
		if fold[0] == "x" {
			folds = append(folds, coord)
		} else {
			folds = append(folds, -coord)
		}
	}

	return paper, folds
}
