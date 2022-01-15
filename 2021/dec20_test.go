package aoc2021

import (
	"io/ioutil"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/tomasaschan/advent-of-code-2021/pkg/dec20"
)

var _ = Describe("Dec 20", func() {
	Context("sample", func() {
		input := `..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###`

		It("solves part a", func() {
			Expect(dec20.A(input)).To(Equal(35))
		})

		It("solves part b", func() {
			Expect(dec20.B(input)).To(Equal(0))
		})
	})

	Context("real input", func() {
		bytes, err := ioutil.ReadFile("input/dec20.txt")

		It("reads input OK", func() {
			Expect(err).NotTo(HaveOccurred())
		})

		input := string(bytes)

		It("solves part a", func() {
			Expect(dec20.A(input)).To(Equal(0))
		})

		It("solves part b", func() {
			Expect(dec20.B(input)).To(Equal(0))
		})
	})
})
