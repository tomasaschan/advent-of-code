package dec11

import (
	"fmt"

	"github.com/fatih/color"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/twod"
)

func A(input string) int {
	octopi := twod.MapFromString(input, twod.Ints())

	flashes := 0
	for range [100]int{} {
		flashes += step(octopi)
	}

	return flashes
}

func B(input string) int {
	octopi := twod.MapFromString(input, twod.Ints())
	ul, lr := octopi.Corners()
	octopiCount := (1 + lr.X - ul.X) * (1 + lr.Y - ul.Y)

	i := 1
	for {
		if step(octopi) == octopiCount {
			return i
		}
		i++
	}
}

func step(octopi twod.Map) int {
	increase(octopi)
	flashing := flash(octopi)
	flashes := flashCount(flashing)
	reset(octopi, flashing)
	return flashes
}

func increase(octopi twod.Map) {
	upperLeft, lowerRight := octopi.Corners()
	for x := upperLeft.X; x <= lowerRight.X; x++ {
		for y := upperLeft.Y; y <= lowerRight.Y; y++ {
			p := twod.Vector{X: x, Y: y}
			energy := octopi.At(p).(int)
			octopi.UpdateAt(p, energy+1)
		}
	}
}

func flash(octopi twod.Map) twod.Map {
	upperLeft, lowerRight := octopi.Corners()
	flashing := twod.BlankMap(upperLeft, lowerRight, false)

	for {
		flashedAny := false
		for x := upperLeft.X; x <= lowerRight.X; x++ {
			for y := upperLeft.Y; y <= lowerRight.Y; y++ {
				p := twod.Vector{X: x, Y: y}
				energy := octopi.At(p).(int)
				flashed := flashing.At(p).(bool)
				if energy > 9 && !flashed {
					flashing.UpdateAt(p, true)
					flashedAny = true
					for _, q := range p.Surroundings() {
						if neighbor := octopi.At(q); neighbor != nil {
							octopi.UpdateAt(q, neighbor.(int)+1)
						}
					}
					for _, q := range p.DiagonalSurroundings() {
						if neighbor := octopi.At(q); neighbor != nil {
							octopi.UpdateAt(q, neighbor.(int)+1)
						}
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

func flashCount(flashing twod.Map) int {
	count := 0
	upperLeft, lowerRight := flashing.Corners()
	for x := upperLeft.X; x <= lowerRight.X; x++ {
		for y := upperLeft.Y; y <= lowerRight.Y; y++ {
			if flashing.At(twod.Vector{X: x, Y: y}).(bool) {
				count += 1
			}
		}
	}
	return count
}

func reset(octopi twod.Map, flashing twod.Map) {
	upperLeft, lowerRight := flashing.Corners()

	for x := upperLeft.X; x <= lowerRight.X; x++ {
		for y := upperLeft.Y; y <= lowerRight.Y; y++ {
			p := twod.Vector{X: x, Y: y}
			if flashing.At(p).(bool) {
				octopi.UpdateAt(p, 0)
			}
		}
	}
}

func show(octopi twod.Map, flashing twod.Map) {
	color.NoColor = false
	upperLeft, lowerRight := octopi.Corners()
	flashColor := color.New(color.FgGreen)
	overflow := color.New(color.FgRed)
	for y := upperLeft.Y; y <= lowerRight.Y; y++ {
		for x := upperLeft.X; x <= lowerRight.X; x++ {
			p := twod.Vector{X: x, Y: y}
			e := octopi.At(p).(int)

			if flashing.At(p).(bool) {
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
