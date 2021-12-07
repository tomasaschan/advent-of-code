package aoc2021

import (
	"math"
	"sort"

	"github.com/tomasaschan/advent-of-code-2021/pkg/utils"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/ints"
)

func dec7_a(input string) int {
	return findBestAlignmentCost(
		utils.AllInts(input),
		func(d int) int { return d },
	)
}

func dec7_b(input string) int {
	return findBestAlignmentCost(
		utils.AllInts(input),
		memoize(fuelToGo),
	)
}

func findBestAlignmentCost(positions []int, fuelCost FuncIntInt) int {
	sort.Ints(positions)
	best := math.MaxInt
	for i := 0; i <= positions[len(positions)-1]; i++ {
		fuelToHere := fuelTo(positions, i, fuelCost)
		if fuelToHere < best {
			best = fuelToHere
		} else {
			break
		}
	}

	return best

}

func fuelTo(positions []int, x int, cost FuncIntInt) int {
	f := 0
	for i := range positions {
		f += cost(ints.Abs(positions[i] - x))
	}
	return f
}

func fuelToGo(d int) int {
	f := 0
	for i := 1; i <= d; i++ {
		f += i
	}
	return f
}

type FuncIntInt func(int) int

func memoize(f FuncIntInt) FuncIntInt {
	cache := map[int]int{}

	return func(i int) int {
		if j, ok := cache[i]; ok {
			return j
		}

		j := f(i)
		cache[i] = j
		return j
	}
}
