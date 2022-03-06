package cuboid

import (
	"fmt"
	"strings"
)

type cuboidSet struct {
	m map[string]Cuboid
}

type CuboidSet interface {
	Add(Cuboid)
	With(Cuboid) CuboidSet
	Remove(Cuboid)
	Without(Cuboid) CuboidSet
	Contains(Cuboid) bool
	fmt.Stringer
}

func NewSet() CuboidSet {
	return &cuboidSet{m: map[string]Cuboid{}}
}

func (s *cuboidSet) Add(c Cuboid) {
	s.m[c.String()] = c
}

func (s *cuboidSet) Remove(c Cuboid) {
	delete(s.m, c.String())
}

func (s *cuboidSet) Contains(c Cuboid) bool {
	_, ok := s.m[c.String()]
	return ok
}

func (s cuboidSet) With(c Cuboid) CuboidSet {
	result := NewSet()
	for _, v := range s.m {
		result.Add(v)
	}
	result.Add(c)
	return result
}

func (s cuboidSet) Without(c Cuboid) CuboidSet {
	result := NewSet()
	for _, v := range s.m {
		if v != c {
			result.Add(v)
		}
	}
	return result
}

func (s cuboidSet) String() string {
	elements := make([]string, 0)

	for k := range s.m {
		elements = append(elements, k)
	}

	return "{" + strings.Join(elements, ", ") + "}"
}
