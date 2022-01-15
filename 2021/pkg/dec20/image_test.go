package dec20

import (
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("algoIndex", func() {
	image := parseImage(`#..#.
#....
##..#
..#..
..###`)

	It("correctly gets index for middle pixel", func() {
		Expect(image.algoIndex(2, 2)).To(Equal(34))
	})
})

var _ = Describe("enhance", func() {
	input := `..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###
`

	It("enhances the middle pixel correctly", func() {
		algo, image := parse(input)
		Expect(algo[image.algoIndex(2, 2)]).To(Equal(1))
	})

	It("enhances image correctly", func() {
		expected := `.##.##.
#..#.#.
##.#..#
####..#
.#..##.
..##..#
...#.#.
`
		algo, image := parse(input)
		enhanced := image.enhance(algo).String()
		Expect(enhanced).To(Equal(expected), enhanced)
	})

	It("enhances twice correctly", func() {
		expected := `.......#.
.#..#.#..
#.#...###
#...##.#.
#.....#.#
.#.#####.
..#.#####
...##.##.
....###..
`

		algo, image := parse(input)
		image = image.enhance(algo)
		image = image.enhance(algo)

		Expect(image.String()).To(Equal(expected), image.String())
	})
})
