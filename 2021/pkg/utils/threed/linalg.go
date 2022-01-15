package threed

import "fmt"

type Vector Point

func (v *Vector) Add(other *Vector) *Vector {
	return &Vector{v.X + other.X, v.Y + other.Y, v.Z + other.Z}
}
func (v *Vector) Subtract(other *Vector) *Vector {
	return &Vector{v.X - other.X, v.Y - other.Y, v.Z - other.Z}
}
func (v *Vector) Scale(k int) *Vector {
	return &Vector{k * v.X, k * v.Y, k * v.Z}
}

type cols []row
type row []int

type matrix struct {
	data cols
}

func I() *matrix { return Matrix(1, 0, 0, 0, 1, 0, 0, 0, 1) }

func Matrix(data ...int) *matrix {
	if len(data) != 9 {
		panic("Must supply exactly 9 data points, in row-major order")
	}
	return &matrix{
		data: []row{
			data[0:3],
			data[3:6],
			data[6:9],
		},
	}
}

func (m *matrix) at(i, j int) int {
	if i < 1 || i > 3 || j < 1 || j > 3 {
		panic(fmt.Sprintf("index %d,%d is out of range; both must be in [1,3]", i, j))
	}

	return m.data[j-1][i-1]
}

func (m *matrix) times(v *Vector) *Vector {
	return &Vector{
		m.at(1, 1)*v.X + m.at(2, 1)*v.Y + m.at(3, 1)*v.Z,
		m.at(1, 2)*v.X + m.at(2, 2)*v.Y + m.at(3, 2)*v.Z,
		m.at(1, 3)*v.X + m.at(2, 3)*v.Y + m.at(3, 3)*v.Z,
	}
}

func (a *matrix) timesM(b *matrix) *matrix {
	elem := func(i, j int) int {
		return a.at(i, 1)*b.at(1, j) +
			a.at(i, 2)*b.at(2, j) +
			a.at(i, 3)*b.at(3, j)
	}

	return Matrix(
		elem(1, 1), elem(2, 1), elem(3, 1),
		elem(1, 2), elem(2, 2), elem(3, 2),
		elem(1, 3), elem(2, 3), elem(3, 3),
	)
}
