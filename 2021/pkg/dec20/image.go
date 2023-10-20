package dec20

import (
	"fmt"
	"math"
	"strings"

	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/twod"
)

type image struct {
	pixels twod.IntMap
	rim    int
}

type algorithm []int

func (img image) enhance(algo algorithm) image {
	ul, lr := img.pixels.Corners()
	nul, nlr := ul.LeftOf().Above(), lr.RightOf().Below()

	var rim int
	if img.rim == 1 {
		rim = algo[0b111_111_111]
	} else {
		rim = algo[0]
	}

	newPixels := twod.BlankIntMap(nul, nlr, rim)

	for y := nul.Y; y <= nlr.Y; y++ {
		for x := nul.X; x <= nlr.X; x++ {
			i := img.algoIndex(x, y)
			newPixels.UpdateAt(twod.Vector{X: x, Y: y}, algo[i])
		}
	}

	newImg := image{
		pixels: newPixels,
		rim:    rim,
	}
	return newImg
}

func (img image) algoIndex(x, y int) int {
	ix := 0
	p := twod.Vector{X: x, Y: y}

	for i, q := range []twod.Vector{
		p.LeftOf().Above(), p.Above(), p.RightOf().Above(),
		p.LeftOf(), p, p.RightOf(),
		p.LeftOf().Below(), p.Below(), p.RightOf().Below(),
	} {
		if pxl := img.pixels.At(q); (pxl != nil && *pxl == 1) || (pxl == nil && img.rim == 1) {
			ix += 1 << (8 - i)
		}
	}

	return ix
}

func (img image) brightPixels() int {
	if img.rim == 1 {
		return math.MaxInt
	}
	c := 0
	ul, lr := img.pixels.Corners()

	for x := ul.X; x <= lr.X; x++ {
		for y := ul.Y; y <= lr.Y; y++ {
			if *img.pixels.At(twod.Vector{X: x, Y: y}) == 1 {
				c++
			}
		}
	}

	return c
}

func (img image) String() string {
	ul, lr := img.pixels.Corners()
	sb := strings.Builder{}

	for y := ul.Y; y <= lr.Y; y++ {
		for x := ul.X; x <= lr.X; x++ {
			if *img.pixels.At(twod.Vector{X: x, Y: y}) == 1 {
				sb.WriteRune('#')
			} else {
				sb.WriteRune('.')
			}
		}
		sb.WriteRune('\n')
	}

	return sb.String()
}

func parse(input string) (algorithm, image) {
	parts := strings.Split(strings.TrimSpace(input), "\n\n")
	return parseAlgorithm(parts[0]), parseImage(parts[1])
}
func parseAlgorithm(input string) algorithm {
	if len(input) != 512 {
		panic(fmt.Sprintf("algorithm input string was not 512 characters long, but %d", len(input)))
	}
	a := make(algorithm, 512)
	for i, c := range input {
		switch c {
		case '.':
			a[i] = 0
		case '#':
			a[i] = 1
		default:
			panic(fmt.Sprintf("unknown character in algorithm at index %d: '%c'", i, c))
		}
	}
	return a
}
func parseImage(input string) image {
	return image{
		pixels: twod.IntMapFromString(input, func(c rune) int {
			switch c {
			case '.':
				return 0
			case '#':
				return 1
			default:
				panic(fmt.Sprintf("unknown character in image at '%c'", c))
			}

		}),
	}
}
