package aoc2021

import (
	"io/ioutil"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/ginkgo/extensions/table"
	. "github.com/onsi/gomega"

	"github.com/tomasaschan/advent-of-code-2021/pkg/dec18"
	"github.com/tomasaschan/advent-of-code-2021/pkg/dec18/snailfish"
)

var _ = Describe("Dec 18", func() {
	DescribeTable("summing lists",
		func(input string, expected string) {
			Expect(dec18.Sum(dec18.Parse(input))).To(Equal(snailfish.Parse(expected)))
		},
		Entry("first", "[1,1]\n[2,2]\n[3,3]\n[4,4]\n", "[[[[1,1],[2,2]],[3,3]],[4,4]]"),
		Entry("second", "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]\n", "[[[[3,0],[5,3]],[4,4]],[5,5]]"),
		Entry("third", "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]\n[6,6]\n", "[[[[5,0],[7,4]],[5,5]],[6,6]]"),
		Entry("slightly larger",
			`[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]
`,
			"[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"),
	)
	Context("example homework", func() {
		input := `[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
`

		It("solves part a", func() {
			Expect(dec18.A(input)).To(Equal(4140))
		})

		It("solves part b", func() {
			Expect(dec18.B(input)).To(Equal(3993))
		})
	})

	Context("real input", func() {
		bytes, err := ioutil.ReadFile("input/dec18.txt")

		It("reads input OK", func() {
			Expect(err).NotTo(HaveOccurred())
		})

		input := string(bytes)

		It("solves part a", func() {
			Expect(dec18.A(input)).To(Equal(4235))
		})

		It("solves part b", func() {
			Expect(dec18.B(input)).To(Equal(4659))
		})
	})
})
