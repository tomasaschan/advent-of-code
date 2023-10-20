package snailfish

import (
	"testing"

	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"
)

func TestSnailfishNumbers(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "snailfish numbers")
}

var _ = DescribeTable("explode",
	func(input string, expected string) {
		Expect(Parse(input).explode().String()).To(Equal(expected))
	},
	Entry("no explosion", "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]", "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"),
	Entry("[[[[[9,8],1],2],3],4]", "[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]"),
	Entry("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]"),
	Entry("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]"),
	Entry("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"),
	Entry("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"),
)

var _ = DescribeTable("split",
	func(input string, expected string) {
		Expect(Parse(input).split().String()).To(Equal(expected))
	},
	Entry("10", "[10,2]", "[[5,5],2]"),
	Entry("11", "[2,11]", "[2,[5,6]]"),
	Entry("12", "[1,[12,3]]", "[1,[[6,6],3]]"),
	Entry("two big ones", "[[2,14],[12,3]]", "[[2,[7,7]],[12,3]]"),
)

var _ = DescribeTable("reduce",
	func(input string, expected string) {
		Expect(Parse(input).reduce().String()).To(Equal(expected))
	},
	Entry("after addition", "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]", "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"),
)

var _ = DescribeTable("magnitude",
	func(input string, expected int) {
		Expect(Parse(input).Magnitude()).To(Equal(expected))
	},
	Entry("[[1,2],[[3,4],5]]", "[[1,2],[[3,4],5]]", 143),
	Entry("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", 1384),
	Entry("[[[[1,1],[2,2]],[3,3]],[4,4]]", "[[[[1,1],[2,2]],[3,3]],[4,4]]", 445),
	Entry("[[[[3,0],[5,3]],[4,4]],[5,5]]", "[[[[3,0],[5,3]],[4,4]],[5,5]]", 791),
	Entry("[[[[5,0],[7,4]],[5,5]],[6,6]]", "[[[[5,0],[7,4]],[5,5]],[6,6]]", 1137),
	Entry("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", 3488),
)
