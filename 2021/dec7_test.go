package aoc2021

import (
	"io/ioutil"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("Dec 7", func() {
	Context("sample", func() {
		input := "16,1,2,0,4,2,7,1,2,14"
		It("solves part a", func() {
			Expect(dec7_a(input)).To(Equal(37))
		})

		It("solves part b", func() {
			Expect(dec7_b(input)).To(Equal(168))
		})
	})

	Context("real input", func() {
		input, err := ioutil.ReadFile("input/dec7.txt")
		Expect(err).NotTo(HaveOccurred())

		It("solves part a", func() {
			Expect(dec7_a(string(input))).To(Equal(357353))
		})

		It("solves part b", func() {
			Expect(dec7_b(string(input))).To(Equal(104822130))
		})
	})
})
