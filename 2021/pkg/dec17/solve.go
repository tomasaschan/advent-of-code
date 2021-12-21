package dec17

import (
	"fmt"
	"math"
	"strings"

	"github.com/tomasaschan/advent-of-code-2021/pkg/utils"
)

func A(input string) int {
	_, _, miny, maxy := parse(input)
	dy0 := findMaxDy0(miny, maxy)
	return maxY(dy0)
}

func B(input string) int {
	_, _, miny, maxy := parse(input)

	fmt.Println()

	for dy0 := 0; dy0 < 200; dy0++ {
		tmax := int(math.Floor(solveForTime(miny, dy0)))
		tmin := int(math.Ceil(solveForTime(maxy, dy0)))
		fmt.Printf("dy0=%d, t ∈ [%d,%d], y ∈ [%d,%d], dy ∈ [%d,%d]\n", dy0, tmin, tmax, tmax*(dy0-tmax), tmin*(dy0-tmin), dy0-tmin, dy0-tmax)
	}
	return 1
}

// ASSUMPTION: target area is completely below y=0
// 			   so we'll pass through ymax on the way in, and ymin on the way out
func solveForTime(y, dy0 int) float64 {
	root := math.Sqrt(float64(dy0*dy0)/4 - float64(y))
	if math.IsNaN(root) {
		return math.NaN()
	}
	a := float64(dy0)/2 + root
	b := float64(dy0)/2 - root
	if a > 0 && b > 0 {
		panic(fmt.Sprintf("y=%d, dy0=%d yielded two solutions! %.2f and %.2f", y, dy0, a, b))
	}
	return a
}

func maxY(dy0 int) int {
	y := 0
	for dy := dy0; dy > 0; dy-- {
		y += dy
	}
	return y
}

func findDy0s(min, max, mindy0, maxdy0 int) []int {
	result := make([]int, 0)
	for dy0 := mindy0; dy0 <= maxdy0; dy0++ {
		if hitsTargetY(min, max, dy0) {
			result = append(result, dy0)
		}
	}
	return result
}

func findMaxDy0(min, max int) int {
	maxdy0 := 0
	for _, dy0 := range findDy0s(min, max, 0, 100) {
		if dy0 > maxdy0 {
			maxdy0 = dy0
		}
	}
	return maxdy0
}

func hitsTargetY(min, max, dy0 int) bool {
	dy := dy0
	for y := 0; min <= y || 0 <= dy; y += dy {
		if min < y && y < max {
			return true
		}
		dy--
	}
	return false
}

func parse(input string) (int, int, int, int) {
	boundaries := utils.AllInts(strings.TrimSpace(input))
	var minx, miny, maxx, maxy int
	if boundaries[0] < boundaries[1] {
		minx, maxx = boundaries[0], boundaries[1]
	} else {
		minx, maxx = boundaries[1], boundaries[0]
	}
	if boundaries[2] < boundaries[3] {
		miny, maxy = boundaries[2], boundaries[3]
	} else {
		miny, maxy = boundaries[3], boundaries[2]
	}
	return minx, maxx, miny, maxy
}
