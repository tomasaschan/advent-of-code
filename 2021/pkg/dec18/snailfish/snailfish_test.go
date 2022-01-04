package snailfish

import (
	"testing"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/ginkgo/extensions/table"
	. "github.com/onsi/gomega"
)

func TestSnailfishNumbers(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "snailfish numbers")
}

var _ = Describe("Snailfish Numbers", func() {
	DescribeTable("explode",
		func(input string, expected string) {
			Expect(parse1(input).explode().String()).To(Equal(expected))
		},
		Entry("no explosion", "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]", "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"),
		Entry("[[[[[9,8],1],2],3],4]", "[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]"),
		Entry("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]"),
		Entry("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]"),
		Entry("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"),
		Entry("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"),
	)
})
