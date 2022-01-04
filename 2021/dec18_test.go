package aoc2021

import (
	"io/ioutil"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/tomasaschan/advent-of-code-2021/pkg/dec18"
)

var _ = Describe("Dec 18", func() {
	Context("sample", func() {
		input := ``

		It("solves part a", func() {
			Expect(dec18.A(input)).To(Equal(0))
		})

		It("solves part b", func() {
			Expect(dec18.B(input)).To(Equal(0))
		})
	})

	Context("real input", func() {
		bytes, err := ioutil.ReadFile("input/dec18.txt")
		
		It("reads input OK", func() {
			Expect(err).NotTo(HaveOccurred())
		})

		input := string(bytes)

		It("solves part a", func() {
			Expect(dec18.A(input)).To(Equal(0))
		})

		It("solves part b", func() {
			Expect(dec18.B(input)).To(Equal(0))
		})
	})
})
