package aoc2021

import (
	"io/ioutil"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/tomasaschan/advent-of-code-2021/pkg/dec11"
)

var _ = Describe("Dec 11", func() {
	Context("sample", func() {
		input := `5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
`

		It("solves part a", func() {
			Expect(dec11.A(input)).To(Equal(1656))
		})

		It("solves part b", func() {
			Expect(dec11.B(input)).To(Equal(195))
		})
	})

	Context("real input", func() {
		bytes, err := ioutil.ReadFile("input/dec11.txt")

		It("reads input OK", func() {
			Expect(err).NotTo(HaveOccurred())
		})

		input := string(bytes)

		It("solves part a", func() {
			Expect(dec11.A(input)).To(Equal(1743))
		})

		It("solves part b", func() {
			Expect(dec11.B(input)).To(Equal(364))
		})
	})
})
