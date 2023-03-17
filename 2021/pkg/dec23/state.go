package dec23

import "github.com/tomasaschan/advent-of-code-2021/pkg/utils/twod"

type State struct {
	Amphipods map[twod.Point]rune
	Depth     int
}

func (s *State) PatchB() {
	s.Depth = 4
	s.Amphipods[twod.Point{2, 4}] = s.Amphipods[twod.Point{2, 2}]
	s.Amphipods[twod.Point{2, 2}] = 'D'
	s.Amphipods[twod.Point{2, 3}] = 'D'
	s.Amphipods[twod.Point{4, 4}] = s.Amphipods[twod.Point{4, 2}]
	s.Amphipods[twod.Point{4, 2}] = 'C'
	s.Amphipods[twod.Point{4, 3}] = 'B'
	s.Amphipods[twod.Point{6, 4}] = s.Amphipods[twod.Point{6, 2}]
	s.Amphipods[twod.Point{6, 2}] = 'B'
	s.Amphipods[twod.Point{6, 3}] = 'A'
	s.Amphipods[twod.Point{8, 4}] = s.Amphipods[twod.Point{8, 2}]
	s.Amphipods[twod.Point{8, 2}] = 'A'
	s.Amphipods[twod.Point{8, 3}] = 'C'
}

func (s *State) IsOrganized() bool {
	for a, x := range SideRooms {
		for y := 1; y <= s.Depth; y++ {
			if s.Amphipods[twod.Point{x, y}] != a {
				return false
			}
		}
	}
	return true
}

var SideRooms = map[rune]int{'A': 2, 'B': 4, 'C': 6, 'D': 8}
var Costs = map[rune]int{'A': 1, 'B': 10, 'C': 100, 'D': 1000}
