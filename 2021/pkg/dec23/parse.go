package dec23

import (
	"strings"

	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/twod"
)

func Parse(input string) *State {
	state := State{
		Amphipods: map[twod.Point]rune{},
		Depth:     2,
	}
	lines := strings.Split(input, "\n")

	for x, c := range lines[1][1 : len(lines[1])-1] {
		if !strings.ContainsRune("# ", c) {
			state.Amphipods[twod.Point{x, 0}] = c
		}
	}

	for _, x := range [4]int{2, 4, 6, 8} {
		for _, y := range [2]int{1, 2} {
			c := rune(lines[y+1][x+1])
			if !strings.ContainsRune("# ", c) {
				state.Amphipods[twod.Point{x, y}] = c
			}
		}
	}

	return &state
}
