package aoc2021

import (
	"io/ioutil"

	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"

	"github.com/tomasaschan/advent-of-code-2021/pkg/dec15"
)

var _ = Describe("Dec 15", func() {
	Context("sample", func() {
		input := `1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
`

		It("solves part a", func() {
			Expect(dec15.A(input)).To(Equal(40))
		})

		It("solves part b", func() {
			Expect(dec15.B(input)).To(Equal(315))
		})
	})

	Context("real input", func() {
		bytes, err := ioutil.ReadFile("input/dec15.txt")

		It("reads input OK", func() {
			Expect(err).NotTo(HaveOccurred())
		})

		input := string(bytes)

		It("solves part a", func() {
			Expect(dec15.A(input)).To(Equal(386))
		})

		It("solves part b", func() {
			Expect(dec15.B(input)).To(Equal(2806))
		})
	})
})
