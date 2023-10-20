package dec20_test

import (
	"io/ioutil"
	"testing"

	. "github.com/onsi/ginkgo/v2"
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
			Expect(dec20.B(input)).To(Equal(3351))
		})
	})

	Context("real input", func() {
		bytes, err := ioutil.ReadFile("../../input/dec20.txt")

		It("reads input OK", func() {
			Expect(err).NotTo(HaveOccurred())
		})

		input := string(bytes)

		It("solves part a", func() {
			Expect(dec20.A(input)).To(Equal(5275))
		})

		It("solves part b", func() {
			Expect(dec20.B(input)).To(Equal(16482))
		})
	})
})

func TestDec20(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "Dec 20")
}
