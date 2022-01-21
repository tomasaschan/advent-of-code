package aoc2021

import (
	"os"

	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"
	"github.com/tomasaschan/advent-of-code-2021/pkg/dec2"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils"
)

var _ = Describe("Dec 2", func() {
	Context("sample", func() {
		input := []string{
			"forward 5",
			"down 5",
			"forward 8",
			"up 3",
			"down 8",
			"forward 2",
		}

		It("solves a", func() {
			Expect(dec2.A(input)).To(Equal(150))
		})
		It("solves b", func() {
			Expect(dec2.B(input)).To(Equal(900))
		})
	})

	Context("real input", func() {
		f, err := os.Open("input/dec2.txt")
		Expect(err).NotTo(HaveOccurred())
		input, err := utils.ReadInput(f)
		Expect(err).NotTo(HaveOccurred())

		It("solves a", func() {
			Expect(dec2.A(input)).To(Equal(2039256))
		})
		It("solves b", func() {
			Expect(dec2.B(input)).To(Equal(1856459736))
		})

	})
})
