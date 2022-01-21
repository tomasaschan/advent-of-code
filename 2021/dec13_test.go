package aoc2021

import (
	"io/ioutil"

	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"

	"github.com/tomasaschan/advent-of-code-2021/pkg/dec13"
)

var _ = Describe("Dec 13", func() {
	Context("sample", func() {
		input := `6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
`

		It("solves part a", func() {
			Expect(dec13.A(input)).To(Equal(17))
		})

		It("solves part b", func() {
			Expect(dec13.B(input)).To(Equal(`#####
#...#
#...#
#...#
#####
.....
.....
`))
		})
	})

	Context("real input", func() {
		bytes, err := ioutil.ReadFile("input/dec13.txt")

		It("reads input OK", func() {
			Expect(err).NotTo(HaveOccurred())
		})

		input := string(bytes)

		It("solves part a", func() {
			Expect(dec13.A(input)).To(Equal(720))
		})

		It("solves part b", func() {
			Expect(dec13.B(input)).To(Equal(`.##..#..#.###..###..###...##..#..#.####.
#..#.#..#.#..#.#..#.#..#.#..#.#..#....#.
#..#.####.#..#.#..#.#..#.#..#.#..#...#..
####.#..#.###..###..###..####.#..#..#...
#..#.#..#.#....#.#..#....#..#.#..#.#....
#..#.#..#.#....#..#.#....#..#..##..####.
`))
		})
	})
})
