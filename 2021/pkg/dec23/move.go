package dec23

import (
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/collections"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/ints"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/twod"
	"golang.org/x/exp/slices"
)

func (s *State) isValidStopLocation(amphipod rune, p twod.Point) bool {
	if p.Y == 0 {
		// must not stop right outside side room entrances
		return p.X != 2 && p.X != 4 && p.X != 6 && p.X != 8
	}

	if SideRooms[amphipod] != p.X {
		// must stop in own sideroom
		return false
	}

	for j := p.Y + 1; p.Y < s.Depth; p.Y++ {
		if s.Amphipods[twod.Point{p.X, j}] != amphipod {
			// must stop at bottom of own side room
			return false
		}
	}

	return true
}

func (s *State) isValidMove(from, to twod.Point) bool {
	if from == to {
		return false // must move
	}
	if from.Y == 0 && to.Y == 0 {
		return false // must not move from hallway to hallway
	}
	if from.X == to.X {
		return false // must not move within same sideroom
	}
	a := s.Amphipods[from]
	if a == 0 {
		return false // can't move from empty space
	}
	return s.isValidStopLocation(a, to)
}

type Move struct {
	From twod.Point
	To   twod.Point
	Cost int
}

func (s *State) ValidMoves(from twod.Point) <-chan twod.Point {
	allMoves := make([]twod.Point, 0)

	q := collections.NewDeque[twod.Point]()
	q.Push(from)
	seen := collections.EmptySet[twod.Point]()

	for !q.Empty() {
		p, _ := q.Pop()

		if s.isValidMove(from, p) {
			allMoves = append(allMoves, p)
		}
		for _, n := range p.Surroundings() {
			if a, ok := s.Amphipods[n]; ok && a == '.' && !seen.Has(n) {
				q.Push(n)
				seen.Add(n)
			}
		}
	}

	moves := make(chan twod.Point)

	go func() {
		defer close(moves)
		i := slices.IndexFunc(allMoves, func(p twod.Point) bool { return p.Y > 0 })
		if i != -1 {
			moves <- allMoves[i]
		} else {
			for _, m := range allMoves {
				moves <- m
			}
		}
	}()

	return moves
}

func (s *State) AllValidMoves() <-chan Move {
	ch := make(chan Move)

	go func() {
		defer close(ch)
		for p, a := range s.Amphipods {
			for q := range s.ValidMoves(p) {
				steps := ints.Abs(q.X-p.X) + p.Y + q.Y

				ch <- Move{
					From: p,
					To:   q,
					Cost: Costs[a] * steps,
				}
			}
		}
	}()

	return ch
}

func (s *State) Move(m Move) *State {
	t := &State{
		Amphipods: make(map[twod.Vector]rune),
		Depth:     s.Depth,
	}
	for p, a := range s.Amphipods {
		if p == m.From {
			t.Amphipods[p] = '.'
		} else {
			t.Amphipods[p] = a
		}
	}
	t.Amphipods[m.To] = s.Amphipods[m.From]
	return t
}
