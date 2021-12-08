package aoc2021

import (
	"io/ioutil"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	"github.com/tomasaschan/advent-of-code-2021/pkg/dec5"
)

var _ = Describe("Dec 5", func() {
	Context("sample", func() {
		input := `0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
`

		It("solves part a", func() {
			Expect(dec5.A(input)).To(Equal(5))
		})

		It("solves part b", func() {
			Expect(dec5.B(input)).To(Equal(12))
		})
	})

	Context("real input", func() {
		input, err := ioutil.ReadFile("input/dec5.txt")
		Expect(err).NotTo(HaveOccurred())

		It("solves part a", func() {
			Expect(dec5.A(string(input))).To(Equal(6189))
		})

		It("solves part b", func() {
			Expect(dec5.B(string(input))).To(Equal(19164))
		})

	})
})
