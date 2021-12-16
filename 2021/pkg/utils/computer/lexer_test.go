package computer

import (
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("Lexer", func() {
	Describe("parsing a literal packet", func() {
		input := "D2FE28"

		parser, err := NewParserForHex(input)
		Expect(err).NotTo(HaveOccurred())

		packet, err := parser.GetPacket()
		Expect(err).NotTo(HaveOccurred())

		It("parses correct version", func() {
			Expect(packet.Version()).To(Equal(6))
		})
		It("parses correct typeID", func() {
			Expect(packet.TypeID()).To(Equal(4))
		})
		It("parses correct value", func() {
			Expect(packet.Value()).To(Equal(2021))
		})
	})

	Describe("parsing a packet with lengthTypeID 0", func() {
		input := "38006F45291200"

		It("parses packet OK", func() {
			By("initializing parser")
			parser, err := NewParserForHex(input)
			Expect(err).NotTo(HaveOccurred())

			By("parsing packet")
			packet, err := parser.GetPacket()
			Expect(err).NotTo(HaveOccurred())

			By("casting to operator packet")
			operator, ok := packet.(*operator)
			Expect(ok).To(BeTrue())

			By("verifying packet contents")
			Expect(operator.Version()).To(Equal(1))
			Expect(operator.TypeID()).To(Equal(6))
			Expect(operator.lengthTypeID).To(Equal(0))
			Expect(len(operator.subPackets)).To(Equal(2))
			Expect(operator.subPackets[0].Value()).To(Equal(10))
			Expect(operator.subPackets[1].Value()).To(Equal(20))
		})
	})

	Describe("parsing a packet with lengthTypeID 1", func() {
		input := "EE00D40C823060"

		It("parses packet OK", func() {
			By("initializing parser")
			parser, err := NewParserForHex(input)
			Expect(err).NotTo(HaveOccurred())

			By("parsing packet")
			packet, err := parser.GetPacket()
			Expect(err).NotTo(HaveOccurred())

			By("casting to operator packet")
			operator, ok := packet.(*operator)
			Expect(ok).To(BeTrue())

			By("verifying packet contents")
			Expect(operator.Version()).To(Equal(7))
			Expect(operator.TypeID()).To(Equal(3))
			Expect(operator.lengthTypeID).To(Equal(1))
			Expect(len(operator.subPackets)).To(Equal(3))
			Expect(operator.subPackets[0].Value()).To(Equal(1))
			Expect(operator.subPackets[1].Value()).To(Equal(2))
			Expect(operator.subPackets[2].Value()).To(Equal(3))
		})
	})
})
