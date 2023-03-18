package dec25

import (
	"strings"

	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/twod"
)

func A(input string) int {
	seaBottom, X, Y := Parse(input)

	i := 1
	for Move(&seaBottom, X, Y) {
		i++
	}

	return i
}

func Move(seaBottom *map[twod.Point]rune, X, Y int) bool {
	current, next := *seaBottom, make(map[twod.Point]rune)
	moved := false

	// move east-moving cucumbers
	// fmt.Println("MOVING HORIZONTALLY")
	for y := Y - 1; y >= 0; y-- {
		for x := X - 1; x >= 0; x-- {
			u := twod.Point{(X + x - 1) % X, y}
			v := twod.Point{x, y}
			w := twod.Point{(x + 1) % X, y}
			if current[u] == '>' && current[v] == '.' {
				next[v] = '>'
				moved = true
			} else if current[v] == '>' && current[w] == '.' {
				next[v] = '.'
			} else {
				next[v] = current[v]
			}
			// fmt.Printf("%v: %c, %v: %c, %v: %c => %c\n", u, current[u], v, current[v], w, current[w], next[v])
		}
	}

	current, next = next, make(map[twod.Vector]rune)

	// move south-moving cucumbers
	// fmt.Println("MOVING VERTICALLY")
	for x := X - 1; x >= 0; x-- {
		for y := Y - 1; y >= 0; y-- {
			u := twod.Point{x, (Y + y - 1) % Y}
			v := twod.Point{x, y}
			w := twod.Point{x, (y + 1) % Y}
			if current[u] == 'v' && current[v] == '.' {
				next[v] = 'v'
				moved = true
			} else if current[v] == 'v' && current[w] == '.' {
				next[v] = '.'
			} else {
				next[v] = current[v]
			}
			// fmt.Printf("%v: %c, %v: %c, %v: %c => %c\n", u, current[u], v, current[v], w, current[w], next[v])
		}
	}

	*seaBottom = next

	return moved
}

func Show(seaBottom map[twod.Point]rune, X, Y int) string {
	sb := &strings.Builder{}

	for y := 0; y < Y; y++ {
		for x := 0; x < X; x++ {
			sb.WriteRune(seaBottom[twod.Point{x, y}])
		}
		sb.WriteRune('\n')
	}

	return sb.String()
}

func Parse(input string) (map[twod.Point]rune, int, int) {
	seaBottom := map[twod.Point]rune{}
	X, Y := 0, 0
	for y, line := range strings.Split(strings.TrimSpace(input), "\n") {
		Y = y
		for x, c := range line {
			X = x
			seaBottom[twod.Point{x, y}] = c
		}
	}
	X += 1
	Y += 1
	return seaBottom, X, Y
}
