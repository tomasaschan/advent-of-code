package cuboid

import (
	"strings"
)

type CuboidSet []Cuboid

func EmptySet() CuboidSet {
	return CuboidSet([]Cuboid{})
}

func (s CuboidSet) Size() int {
	z := 0
	for _, c := range s {
		z += c.Size()
	}
	return z
}

func (s CuboidSet) IsEmpty() bool {
	for _, c := range s {
		if c.Size() != 0 {
			return false
		}
	}

	return true
}

func (s CuboidSet) With(c CuboidSet) CuboidSet {
	onlyNew := EmptySet()

	return append(c, onlyNew...)
}

func (s CuboidSet) String() string {
	elements := make([]string, 0)

	for _, k := range s {
		elements = append(elements, k.String())
	}

	return "{" + strings.Join(elements, ", ") + "}"
}
