package aoc2021

import (
	"io/ioutil"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/tomasaschan/advent-of-code-2021/pkg/dec14"
)

var _ = Describe("Dec 14", func() {
	Context("sample", func() {
		input := `NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
`

		It("solves part a", func() {
			Expect(dec14.A(input)).To(Equal(1588))
		})

		It("solves part b", func() {
			Expect(dec14.B(input)).To(Equal(2188189693529))
		})
	})

	Context("real input", func() {
		bytes, err := ioutil.ReadFile("input/dec14.txt")

		It("reads input OK", func() {
			Expect(err).NotTo(HaveOccurred())
		})

		input := string(bytes)

		It("solves part a", func() {
			Expect(dec14.A(input)).To(Equal(2233))
		})

		It("solves part b", func() {
			Expect(dec14.B(input)).To(Equal(2884513602164))
		})
	})
})
