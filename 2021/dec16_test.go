package aoc2021

import (
	"io/ioutil"

	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"

	"github.com/tomasaschan/advent-of-code-2021/pkg/dec16"
)

var _ = Describe("Dec 16", func() {
	Context("sample", func() {
		DescribeTable("solves part a",
			func(input string, a int) {
				Expect(dec16.A(input)).To(Equal(a))

			},
			Entry("first", "8A004A801A8002F478", 16),
			Entry("second", "620080001611562C8802118E34", 12),
			Entry("third", "C0015000016115A2E0802F182340", 23),
			Entry("fourth", "A0016C880162017C3686B18A3D4780", 31),
		)
	})

	Context("real input", func() {
		bytes, err := ioutil.ReadFile("input/dec16.txt")

		It("reads input OK", func() {
			Expect(err).NotTo(HaveOccurred())
		})

		input := string(bytes)

		It("solves part a", func() {
			Expect(dec16.A(input)).To(Equal(943))
		})

		It("solves part b", func() {
			Expect(dec16.B(input)).To(Equal(167737115857))
		})
	})
})
