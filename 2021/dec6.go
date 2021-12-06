package aoc2021

import (
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils"
)

func dec6_a(input string) int {
	return solve(input, 80)
}

func dec6_b(input string) int {
	return solve(input, 256)
}

func solve(input string, steps int) int {
	school := school{make([]int, 9)}

	for _, i := range utils.AllInts(input) {
		school.lanternfish[i] += 1
	}

	for i := 0; i < steps; i++ {
		school.evolve()
	}

	return school.size()
}

type school struct {
	lanternfish []int
}

func (s *school) evolve() {
	procreators := s.lanternfish[0]

	s.lanternfish = append(s.lanternfish[1:], procreators)

	s.lanternfish[6] += procreators
}

func (s *school) size() int {
	sz := 0
	for _, f := range s.lanternfish {
		sz += f
	}
	return sz
}
