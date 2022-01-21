package aoc2021

import (
	"io/ioutil"

	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"

	"github.com/tomasaschan/advent-of-code-2021/pkg/dec9"
)

var _ = Describe("Dec 9", func() {
	Context("sample", func() {
		input := `2199943210
3987894921
9856789892
8767896789
9899965678
`

		It("solves part a", func() {
			Expect(dec9.A(input)).To(Equal(15))
		})

		It("solves part b", func() {
			Expect(dec9.B(input)).To(Equal(1134))
		})
	})

	Context("real input", func() {
		bytes, err := ioutil.ReadFile("input/dec9.txt")

		It("reads input OK", func() {
			Expect(err).NotTo(HaveOccurred())
		})

		input := string(bytes)

		It("solves part a", func() {
			Expect(dec9.A(input)).To(Equal(480))
		})

		It("solves part b", func() {
			Expect(dec9.B(input)).To(Equal(1045660))
		})
	})
})
