package twod

import (
	"fmt"
	"strconv"
	"strings"
)

type Map struct {
	terrain map[Vector]interface{}
}

type TerrainFunc func(rune) interface{}

func BlankMap(upperLeft Vector, lowerRight Vector, defaultValue interface{}) Map {
	m := Map{terrain: map[Vector]interface{}{}}
	for x := upperLeft.X; x <= lowerRight.X; x++ {
		for y := upperLeft.Y; y <= lowerRight.Y; y++ {
			m.terrain[Vector{X: x, Y: y}] = defaultValue
		}
	}
	return m
}

func MapFromString(input string, terrain func(rune) interface{}) Map {
	m := Map{terrain: map[Vector]interface{}{}}

	for y, line := range strings.Split(input, "\n") {
		for x, c := range line {
			m.terrain[Vector{x, y}] = terrain(c)
		}
	}

	return m
}

func Ints() TerrainFunc {
	return func(r rune) interface{} {
		i, err := strconv.Atoi(string(r))
		if err != nil {
			panic(err)
		}
		return i
	}
}

func (m Map) At(p Vector) interface{} {
	if t, ok := m.terrain[p]; ok {
		return t
	}
	return nil
}

func (m Map) UpdateAt(p Vector, v interface{}) {
	m.terrain[p] = v
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

func (m Map) Corners() (Vector, Vector) {
	xmin, ymin, xmax, ymax := 0, 0, 0, 0
	for p := range m.terrain {
		if p.X < xmin {
			xmin = p.X
		}
		if p.Y < ymin {
			ymin = p.Y
		}
		if p.X > xmax {
			xmax = p.X
		}
		if p.Y > ymax {
			ymax = p.Y
		}
	}
	return Vector{xmin, ymin}, Vector{xmax, ymax}
}

func (m Map) String() string {
	upperLeft, bottomRight := m.Corners()
	return fmt.Sprintf("map from (%d,%d) to (%d,%d)", upperLeft.X, upperLeft.Y, bottomRight.X, bottomRight.Y)
}
