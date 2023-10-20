package dec16

import (
	"strings"

	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/computer"
)

func A(input string) int {
	packet := readTransmission(input)
	return packet.VersionSum()
}

func B(input string) int {
	packet := readTransmission(input)
	return packet.Value()
}

func readTransmission(input string) computer.Packet {
	parser, err := computer.NewParserForHex(strings.TrimSpace(input))
	if err != nil {
		panic(err)
	}
	packet, err := parser.GetPacket()
	if err != nil {
		panic(err)
	}
	return packet
}
