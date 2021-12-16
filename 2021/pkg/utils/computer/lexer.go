package computer

import (
	"fmt"
	"strconv"
	"strings"
)

var (
	ErrUnknownTypeId = func(typeID int) error { return fmt.Errorf("unknown type id: %d", typeID) }
)

type Lexer interface {
	GetPacket() (Packet, error)
}

var _ Lexer = &parser{}

func (p *parser) GetPacket() (Packet, error) {
	bits, err := p.Consume(3)
	if err != nil {
		return nil, fmt.Errorf("consume packet version: %w", err)
	}
	version, err := p.getIntValue(bits)
	if err != nil {
		return nil, err
	}
	bits, err = p.Consume(3)
	if err != nil {
		return nil, err
	}
	typeID, err := p.getIntValue(bits)
	if err != nil {
		return nil, err
	}

	switch typeID {
	case 4:
		value, err := p.getLiteralValue()
		if err != nil {
			return nil, err
		}
		return &literal{
			meta: meta{
				version: version,
				typeID:  typeID,
			},
			value: value,
		}, nil
	default:
		lengthTypeID, err := p.getLengthTypeID()
		if err != nil {
			return nil, err
		}
		var subpackets []Packet
		if lengthTypeID == 1 {
			subpackets, err = p.getSubPacketsByCount()
			if err != nil {
				return nil, err
			}
		} else {
			subpackets, err = p.getSubPacketsByBits()
			if err != nil {
				return nil, err
			}
		}

		return &operator{
			meta: meta{
				version: version,
				typeID:  typeID,
			},
			lengthTypeID: lengthTypeID,
			subPackets:   subpackets,
		}, nil
	}
}

func (p *parser) getIntValue(bits string) (int, error) {
	i, err := strconv.ParseInt(bits, 2, 16)
	if err != nil {
		return 0, err
	}
	return int(i), nil
}

func (p *parser) getLiteralValue() (int, error) {
	sb := strings.Builder{}
	for {
		part, err := p.Consume(5)
		if err != nil {
			return 0, err
		}
		sb.WriteString(part[1:])
		if part[0] == '0' {
			break
		}
	}
	i, err := strconv.ParseInt(sb.String(), 2, 64)
	if err != nil {
		return 0, err
	}
	return int(i), nil
}

func (p *parser) getLengthTypeID() (int, error) {
	value, err := p.Consume(1)
	if err != nil {
		return 0, fmt.Errorf("reading length type ID: %w", err)
	}
	lengthTypeID, err := p.getIntValue(value)
	if err != nil {
		return 0, fmt.Errorf("parsing length type ID: %w", err)
	}
	return lengthTypeID, nil
}

func (p *parser) getSubPacketsByCount() ([]Packet, error) {
	countBits, err := p.Consume(11)
	if err != nil {
		return nil, fmt.Errorf("consume subpacket count bits: %w", err)
	}
	count, err := p.getIntValue(countBits)
	if err != nil {
		return nil, fmt.Errorf("convert subpacket count bits to count: %w", err)
	}
	result := make([]Packet, count)

	for i := 0; i < count; i++ {
		result[i], err = p.GetPacket()
		if err != nil {
			return nil, fmt.Errorf("read subpacket %d: %w", i, err)
		}
	}

	return result, nil
}

func (p *parser) getSubPacketsByBits() ([]Packet, error) {
	countBits, err := p.Consume(15)
	if err != nil {
		return nil, fmt.Errorf("consume subpacket count bits: %w", err)
	}
	count, err := p.getIntValue(countBits)
	if err != nil {
		return nil, fmt.Errorf("convert subpacket count bits to count: %w", err)
	}
	bits, err := p.Consume(count)
	if err != nil {
		return nil, fmt.Errorf("read subpacket bits: %w", err)
	}

	subparser := NewParserForBin(bits)
	subpackets := make([]Packet, 0)
	for !subparser.IsEOF() {
		p, err := subparser.GetPacket()
		if err != nil {
			return nil, fmt.Errorf("read subpacket: %w", err)
		}
		subpackets = append(subpackets, p)
	}
	return subpackets, nil
}
