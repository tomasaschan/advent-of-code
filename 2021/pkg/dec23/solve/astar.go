package solve

import (
	"github.com/tomasaschan/advent-of-code-2021/pkg/dec23"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/collections"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/ints"
)

func astar(state *dec23.State) int {
	g := map[string]int{}
	g[k(state)] = 0

	f := map[string]int{}
	f[k(state)] = h(state)

	q := collections.NewPriorityQueue[*dec23.State, int]()
	q.Push(state, h(state))

	for !q.Empty() {
		current, cost := q.Pop()

		if current.IsOrganized() {
			return cost
		}

		// fmt.Println(k(current))

		for m := range current.AllValidMoves() {
			neighbor := current.Move(m)
			tentative_g := g[k(current)] + m.Cost
			if gn, ok := g[k(neighbor)]; !ok || tentative_g < gn {
				g[k(neighbor)] = tentative_g
				f[k(neighbor)] = tentative_g + h(neighbor)

				q.Push(neighbor, tentative_g+h(neighbor))
			}
		}
	}

	return -1
}

func k(state *dec23.State) string { return state.Display(dec23.NoWhitespace()) }

func h(state *dec23.State) int {
	cost := 0
	for p, a := range state.Amphipods {
		steps := 0
		if p.X != dec23.SideRooms[a] {
			steps = ints.Abs(dec23.SideRooms[a]-p.X) + p.Y + 1
		}

		cost += steps * dec23.Costs[a]
	}
	return cost
}
