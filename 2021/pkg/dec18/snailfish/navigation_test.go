package snailfish

import (
	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"
)

var _ = Describe("snailfish navigation", func() {
	DescribeTable("neighbor",
		func(input string, path []side, dir side, expected string) {
			root := Parse(input).(*pair)

			actual, ok := root.neighbor(dir, path...)
			Expect(ok).To(Equal(expected != ""))
			if ok {
				Expect(root.at(actual...).String()).To(Equal(expected))
			}

		},
		Entry("nothing to the left", "[[[[[9,8],1],2],3],4]", []side{left, left, left, left}, left, ""),
		Entry("right of a left-pair", "[[[[[9,8],1],2],3],4]", []side{left, left, left, left}, right, "1"),
		Entry("nothing to the right", "[7,[6,[5,[4,[3,2]]]]]", []side{right, right, right, right}, right, ""),
	)

	DescribeTable("leftmostPair4Deep",
		func(input string, path []side, expected string) {
			in := Parse(input).(*pair)

			p, ok := in.leftmostPairAtDepth(4)
			Expect(ok).To(BeTrue())

			Expect(p).To(Equal(path))
			Expect(in.at(p...)).To(Equal(Parse(expected)))
		},
		Entry("[[[[[9,8],1],2],3],4]", "[[[[[9,8],1],2],3],4]", []side{left, left, left, left}, "[9,8]"),
		Entry("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[4,[3,2]]]]]", []side{right, right, right, right}, "[3,2]"),
		Entry("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[4,[3,2]]]],1]", []side{right, right, right, left}, "[3,2]"),
		Entry("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", []side{right, right, right, left}, "[7,3]"),
		Entry("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", []side{right, right, right, right}, "[3,2]"),
	)
})
