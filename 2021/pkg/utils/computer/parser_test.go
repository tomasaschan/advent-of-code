package computer

import (
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/ginkgo/extensions/table"
	. "github.com/onsi/gomega"
)

var _ = Describe("Parser", func() {
	DescribeTable("Converting hex input to binary", func(hex string, binary string) {
		parser, err := NewParserForHex(hex)
		Expect(err).NotTo(HaveOccurred())
		Expect(parser.input).To(Equal(binary))
	},
		Entry("first example", "D2FE28", "110100101111111000101000"),
		Entry("second example", "38006F45291200", "00111000000000000110111101000101001010010001001000000000"),
		Entry("third example", "EE00D40C823060", "11101110000000001101010000001100100000100011000001100000"),
	)
})
