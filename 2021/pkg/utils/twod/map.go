package twod

import (
	"fmt"
	"strconv"
	"strings"
)

type Map interface {
	Size() Vector
	Corners() (Vector, Vector)
	NeighborsOf(Vector) []Vector
	DiagonalNeighborsOf(Vector) []Vector
}
type IntMap interface {
	Map
	At(Vector) *int
}
type WritableIntMap interface {
	IntMap
	UpdateAt(Vector, int)
}

type terrain struct {
	terrain    map[Vector]int
	upperLeft  Vector
	lowerRight Vector
}

type TerrainFunc func(rune) int

func BlankIntMap(upperLeft Vector, lowerRight Vector, defaultValue int) *terrain {
	m := map[Vector]int{}
	for x := upperLeft.X; x <= lowerRight.X; x++ {
		for y := upperLeft.Y; y <= lowerRight.Y; y++ {
			m[Vector{X: x, Y: y}] = defaultValue
		}
	}
	return &terrain{terrain: m, upperLeft: upperLeft, lowerRight: lowerRight}
}

func IntMapFromString(input string, tf func(rune) int) *terrain {
	m := map[Vector]int{}

	maxx, maxy := 0, 0
	for y, line := range strings.Split(input, "\n") {
		if line == "" {
			continue
		}
		for x, c := range line {
			m[Vector{x, y}] = tf(c)
			if x > maxx {
				maxx = x
			}
		}
		if y > maxy {
			maxy = y
		}
	}

	return &terrain{terrain: m, upperLeft: Vector{X: 0, Y: 0}, lowerRight: Vector{X: maxx, Y: maxy}}
}

func Ints() TerrainFunc {
	return func(r rune) int {
		i, err := strconv.Atoi(string(r))
		if err != nil {
			panic(err)
		}
		return i
	}
}

func (m *terrain) At(p Vector) *int {
	if t, ok := m.terrain[p]; ok {
		return &t
	}
	return nil
}

func (m *terrain) UpdateAt(p Vector, v int) {
	m.terrain[p] = v
	if p.X < m.upperLeft.X || p.Y < m.upperLeft.Y {
		m.upperLeft = p
	}
	if p.X > m.lowerRight.X || p.Y > m.lowerRight.Y {
		m.lowerRight = p
	}
}

func (p Vector) Add(v Vector) Vector {
	return Vector{p.X + v.X, p.Y + v.Y}
}

func Up() Vector    { return Vector{0, -1} }
func Down() Vector  { return Vector{0, 1} }
func Right() Vector { return Vector{1, 0} }
func Left() Vector  { return Vector{-1, 0} }

func (v Vector) Above() Vector   { return v.Add(Up()) }
func (v Vector) Below() Vector   { return v.Add(Down()) }
func (v Vector) RightOf() Vector { return v.Add(Right()) }
func (v Vector) LeftOf() Vector  { return v.Add(Left()) }

func (v Vector) Surroundings() []Vector {
	return []Vector{v.Above(), v.RightOf(), v.Below(), v.LeftOf()}
}
func (v Vector) DiagonalSurroundings() []Vector {
	return []Vector{v.LeftOf().Above(), v.RightOf().Above(), v.LeftOf().Below(), v.RightOf().Below()}
}

func (m *terrain) Corners() (Vector, Vector) {
	return m.upperLeft, m.lowerRight
}

func (m *terrain) Size() Vector {
	ul, lr := m.Corners()
	return Vector{X: 1 + lr.X - ul.X, Y: 1 + lr.Y - ul.Y}
}

func (m *terrain) NeighborsOf(v Vector) []Vector {
	result := make([]Vector, 0, 4)
	for _, n := range v.Surroundings() {
		if _, ok := m.terrain[n]; ok {
			result = append(result, n)
		}
	}
	return result
}
func (m *terrain) DiagonalNeighborsOf(v Vector) []Vector {
	result := make([]Vector, 0, 4)
	for _, n := range v.DiagonalSurroundings() {
		if _, ok := m.terrain[n]; ok {
			result = append(result, n)
		}
	}
	return result
}

func (m *terrain) String() string {
	upperLeft, bottomRight := m.Corners()
	return fmt.Sprintf("map from (%d,%d) to (%d,%d)", upperLeft.X, upperLeft.Y, bottomRight.X, bottomRight.Y)
}
