package aoc2021

import (
	"io/ioutil"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"github.com/tomasaschan/advent-of-code-2021/pkg/dec10"
)

var _ = Describe("Dec 10", func() {
	Context("sample", func() {
		input := `[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
`

		It("solves part a", func() {
			Expect(dec10.A(input)).To(Equal(26397))
		})

		It("solves part b", func() {
			Expect(dec10.B(input)).To(Equal(288957))
		})
	})

	Context("real input", func() {
		bytes, err := ioutil.ReadFile("input/dec10.txt")

		It("reads input OK", func() {
			Expect(err).NotTo(HaveOccurred())
		})

		input := string(bytes)

		It("solves part a", func() {
			Expect(dec10.A(input)).To(Equal(345441))
		})

		It("solves part b", func() {
			Expect(dec10.B(input)).To(Equal(3235371166))
		})
	})
})
