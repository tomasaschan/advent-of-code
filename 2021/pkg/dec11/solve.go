package dec11

import (
	"fmt"

	"github.com/fatih/color"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/twod"
)

func A(input string) int {
	octopi := twod.IntMapFromString(input, twod.Ints())

	flashes := 0
	for range [100]int{} {
		flashes += step(octopi)
	}

	return flashes
}

func B(input string) int {
	octopi := twod.IntMapFromString(input, twod.Ints())
	ul, lr := octopi.Corners()
	octopiCount := (1 + lr.X - ul.X) * (1 + lr.Y - ul.Y)

	i := 1
	for {
		if step(octopi) == octopiCount {
			return i
		}
		i++

		if i > 10000 {
			return -1
		}
	}
}

func step(octopi twod.WritableIntMap) int {
	increase(octopi)
	flashing := flash(octopi)
	flashes := flashCount(flashing)
	reset(octopi, flashing)
	return flashes
}

func increase(octopi twod.WritableIntMap) {
	upperLeft, lowerRight := octopi.Corners()
	for x := upperLeft.X; x <= lowerRight.X; x++ {
		for y := upperLeft.Y; y <= lowerRight.Y; y++ {
			p := twod.Vector{X: x, Y: y}
			energy := *octopi.At(p)
			octopi.UpdateAt(p, energy+1)
		}
	}
}

func flash(octopi twod.WritableIntMap) map[twod.Vector]bool {
	upperLeft, lowerRight := octopi.Corners()
	flashing := map[twod.Vector]bool{}

	for {
		flashedAny := false
		for x := upperLeft.X; x <= lowerRight.X; x++ {
			for y := upperLeft.Y; y <= lowerRight.Y; y++ {
				p := twod.Vector{X: x, Y: y}
				energy := *octopi.At(p)
				if energy > 9 && !flashing[p] {
					flashing[p] = true
					flashedAny = true
					for _, q := range octopi.NeighborsOf(p) {
						octopi.UpdateAt(q, *octopi.At(q)+1)
					}
					for _, q := range octopi.DiagonalNeighborsOf(p) {
						octopi.UpdateAt(q, *octopi.At(q)+1)
					}
				}
			}
		}

		if !flashedAny {
			break
		}
	}

	return flashing
}

func flashCount(flashing map[twod.Vector]bool) int {
	count := 0
	for _, f := range flashing {
		if f {
			count++
		}
	}
	return count
}

func reset(octopi twod.WritableIntMap, flashing map[twod.Vector]bool) {
	for p, f := range flashing {
		if f {
			octopi.UpdateAt(p, 0)
		}
	}
}

func show(octopi twod.IntMap, flashing map[twod.Vector]bool) {
	color.NoColor = false
	upperLeft, lowerRight := octopi.Corners()
	flashColor := color.New(color.FgGreen)
	overflow := color.New(color.FgRed)
	for y := upperLeft.Y; y <= lowerRight.Y; y++ {
		for x := upperLeft.X; x <= lowerRight.X; x++ {
			p := twod.Vector{X: x, Y: y}
			e := *octopi.At(p)

			if flashing[p] {
				flashColor.Printf("%d", e%10)
			} else if e >= 10 {
				overflow.Printf("%d", e%10)
			} else {
				fmt.Print(e)
			}
		}
		fmt.Println()
	}
}
