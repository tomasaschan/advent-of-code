package aoc2021

import (
	"os"

	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"

	"github.com/tomasaschan/advent-of-code-2021/pkg/dec1"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils"
)

var _ = Describe("Dec 1", func() {
	Context("sample", func() {
		input := []string{
			"199",
			"200",
			"208",
			"210",
			"200",
			"207",
			"240",
			"269",
			"260",
			"263",
		}

		It("solves a", func() {
			Expect(dec1.A(input)).To(Equal(7))
		})
		It("solves b", func() {
			Expect(dec1.B(input)).To(Equal(5))
		})
	})

	Context("real input", func() {
		f, err := os.Open("input/dec1.txt")
		Expect(err).NotTo(HaveOccurred())
		input, err := utils.ReadInput(f)
		Expect(err).NotTo(HaveOccurred())

		It("solves a", func() {
			Expect(dec1.A(input)).To(Equal(1195))
		})
		It("solves b", func() {
			Expect(dec1.B(input)).To(Equal(1235))
		})

	})
})
