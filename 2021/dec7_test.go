package aoc2021

import (
	"io/ioutil"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
	"github.com/tomasaschan/advent-of-code-2021/pkg/dec7"
)

var _ = Describe("Dec 7", func() {
	Context("sample", func() {
		input := "16,1,2,0,4,2,7,1,2,14"
		It("solves part a", func() {
			Expect(dec7.A(input)).To(Equal(37))
		})

		It("solves part b", func() {
			Expect(dec7.B(input)).To(Equal(168))
		})
	})

	Context("real input", func() {
		input, err := ioutil.ReadFile("input/dec7.txt")
		Expect(err).NotTo(HaveOccurred())

		It("solves part a", func() {
			Expect(dec7.A(string(input))).To(Equal(357353))
		})

		It("solves part b", func() {
			Expect(dec7.B(string(input))).To(Equal(104822130))
		})
	})
})
