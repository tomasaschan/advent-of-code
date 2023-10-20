package threed

type Rotation interface {
	Apply(v *Vector) *Vector
	After(r Rotation) Rotation
}

var (
	AllRotations []Rotation
)

func init() {
	AllRotations = all_rotations()
}

func (m *matrix) Apply(v *Vector) *Vector {
	return m.times(v)
}
func (A *matrix) After(r Rotation) Rotation {
	B := r.(*matrix)
	return B.timesM(A)
}

func RotX() Rotation {
	return Matrix(
		1, 0, 0,
		0, 0, 1,
		0, -1, 0,
	)
}

func RotY() Rotation {
	return Matrix(
		0, 0, -1,
		0, 1, 0,
		1, 0, 0,
	)
}

func RotZ() Rotation {
	return Matrix(
		0, 1, 0,
		-1, 0, 0,
		0, 0, 1,
	)
}

func all_rotations() []Rotation {
	var r Rotation = Matrix(
		1, 0, 0,
		0, 1, 0,
		0, 0, 1,
	)
	x, y, z := RotX(), RotY(), RotZ()
	result := make([]Rotation, 0)

	// all pointing at +x
	for range [4]int{} {
		r = x.After(r)
		result = append(result, r)
	}

	// all pointing at -y
	r = z.After(r)
	for range [4]int{} {
		r = y.After(r)
		result = append(result, r)
	}

	// all pointing at -x
	r = z.After(r)
	for range [4]int{} {
		r = x.After(r)
		result = append(result, r)
	}

	// all pointing at +y
	r = z.After(r)
	for range [4]int{} {
		r = y.After(r)
		result = append(result, r)
	}

	// all pointing at -z
	r = x.After(r)
	for range [4]int{} {
		r = z.After(r)
		result = append(result, r)
	}

	// all pointing at +z
	r = x.After(x.After(r))
	for range [4]int{} {
		r = z.After(r)
		result = append(result, r)
	}

	return result
}
