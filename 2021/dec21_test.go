package aoc2021

import (
	"io/ioutil"

	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"

	"github.com/tomasaschan/advent-of-code-2021/pkg/dec21"
)

var _ = Describe("Dec 21", func() {
	Context("sample", func() {
		input := `Player 1 starting position: 4
Player 2 starting position: 8
`

		It("solves part a", func() {
			Expect(dec21.A(input)).To(Equal(739785))
		})

		It("solves part b", func() {
			Expect(dec21.B(input)).To(Equal(444356092776315))
		})
	})

	Context("real input", func() {
		bytes, err := ioutil.ReadFile("input/dec21.txt")

		It("reads input OK", func() {
			Expect(err).NotTo(HaveOccurred())
		})

		input := string(bytes)

		It("solves part a", func() {
			Expect(dec21.A(input)).To(Equal(752745))
		})

		It("solves part b", func() {
			Expect(dec21.B(input)).To(Equal(309196008717909))
		})
	})
})
