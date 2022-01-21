package aoc2021

import (
	"os"

	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"
	"github.com/tomasaschan/advent-of-code-2021/pkg/dec3"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils"
)

var _ = Describe("Dec 3", func() {
	Context("sample", func() {
		input := []string{
			"00100",
			"11110",
			"10110",
			"10111",
			"10101",
			"01111",
			"00111",
			"11100",
			"10000",
			"11001",
			"00010",
			"01010",
		}

		It("solves a", func() {
			Expect(dec3.A(input)).To(Equal(198))
		})
		It("solves b", func() {
			Expect(dec3.B(input)).To(Equal(230))
		})
	})

	Context("real input", func() {
		f, err := os.Open("input/dec3.txt")
		Expect(err).NotTo(HaveOccurred())
		input, err := utils.ReadInput(f)
		Expect(err).NotTo(HaveOccurred())

		It("solves a", func() {
			Expect(dec3.A(input)).To(Equal(3813416))
		})
		It("solves b", func() {
			Expect(dec3.B(input)).To(Equal(2990784))
		})

	})
})
