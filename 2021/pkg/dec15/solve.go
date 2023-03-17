package dec15

import (
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/collections"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/twod"
)

func A(input string) int {
	m := twod.IntMapFromString(input, twod.Ints())
	return costOfBestPath(m)
}

func B(input string) int {
	m := twod.IntMapFromString(input, twod.Ints())
	e := Expanded{M: m, Nx: 5, Ny: 5}
	return costOfBestPath(e)
}

func costOfBestPath(m twod.IntMap) int {
	_, lr := m.Corners()
	q := collections.NewPriorityQueue[twod.Point, int]()
	seen := map[twod.Vector]bool{}

	q.Push(twod.Vector{X: 0, Y: 0}, 0)
	i := 0
	for !q.Empty() {
		p, risk := q.Pop()

		if p == lr {
			return risk
		}

		for _, n := range m.NeighborsOf(p) {
			if _, ok := seen[n]; !ok {
				seen[n] = true
				q.Push(n, risk+*m.At(n))
			}
		}
		i++
	}

	return -1
}

type Expanded struct {
	M  twod.IntMap
	Nx int
	Ny int
}

var _ twod.IntMap = Expanded{}

func (e Expanded) At(p twod.Vector) *int {
	ul, lr := e.Corners()
	if p.X < ul.X || p.X > lr.X || p.Y < ul.Y || p.Y > lr.Y {
		// out of bounds
		return nil
	}
	s := e.M.Size()
	q := twod.Vector{X: p.X % s.X, Y: p.Y % s.Y}
	v := e.M.At(q)
	if v == nil {
		return nil
	}
	w := (*v+p.X/s.X+p.Y/s.Y-1)%9 + 1
	return &w
}

func (e Expanded) Corners() (twod.Vector, twod.Vector) {
	ul, _ := e.M.Corners()
	s := e.M.Size()
	return ul, twod.Vector{X: s.X*e.Nx - 1, Y: s.Y*e.Ny - 1}
}
func (e Expanded) Size() twod.Vector {
	s := e.M.Size()
	return twod.Vector{X: s.X * e.Nx, Y: s.Y * e.Ny}
}

func (e Expanded) NeighborsOf(p twod.Vector) []twod.Vector {
	result := make([]twod.Vector, 0, 4)
	for _, q := range p.Surroundings() {
		if e.At(q) != nil {
			result = append(result, q)
		}
	}

	return result
}
func (e Expanded) DiagonalNeighborsOf(p twod.Vector) []twod.Vector {
	return e.M.DiagonalNeighborsOf(p)
}
