package dec15

import (
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/twod"
)

func A(input string) int {
	m := twod.IntMapFromString(input, twod.Ints())
	return costOfBestPath(m)
}

func B(input string) int {
	m := twod.IntMapFromString(input, twod.Ints())
	e := expanded{m: m, nx: 5, ny: 5}
	return costOfBestPath(e)
}

func costOfBestPath(m twod.IntMap) int {
	_, lr := m.Corners()
	q := twod.PriorityQueue()
	seen := map[twod.Vector]bool{}

	q.Push(twod.Vector{X: 0, Y: 0}, 0)
	i := 0
	for i < 1e6 {
		p, risk := q.Pop()

		if p == nil {
			return -1
		}
		if *p == lr {
			return risk
		}

		for _, n := range m.NeighborsOf(*p) {
			if _, ok := seen[n]; !ok {
				seen[n] = true
				q.Push(n, risk+*m.At(n))
			}
		}
		i++
	}

	return -1
}

type expanded struct {
	m  twod.IntMap
	nx int
	ny int
}

var _ twod.IntMap = expanded{}

func (e expanded) At(p twod.Vector) *int {
	ul, lr := e.Corners()
	if p.X < ul.X || p.X > lr.X || p.Y < ul.Y || p.Y > lr.Y {
		// out of bounds
		return nil
	}
	s := e.m.Size()
	q := twod.Vector{X: p.X % s.X, Y: p.Y % s.Y}
	v := e.m.At(q)
	if v == nil {
		return nil
	}
	w := (*v+p.X/s.X+p.Y/s.Y-1)%9 + 1
	return &w
}

func (e expanded) Corners() (twod.Vector, twod.Vector) {
	ul, _ := e.m.Corners()
	s := e.m.Size()
	return ul, twod.Vector{X: s.X*e.nx - 1, Y: s.Y*e.ny - 1}
}
func (e expanded) Size() twod.Vector {
	s := e.m.Size()
	return twod.Vector{X: s.X * e.nx, Y: s.Y * e.ny}
}

func (e expanded) NeighborsOf(p twod.Vector) []twod.Vector {
	result := make([]twod.Vector, 0, 4)
	for _, q := range p.Surroundings() {
		if e.At(q) != nil {
			result = append(result, q)
		}
	}

	return result
}
func (e expanded) DiagonalNeighborsOf(p twod.Vector) []twod.Vector {
	return e.m.DiagonalNeighborsOf(p)
}
