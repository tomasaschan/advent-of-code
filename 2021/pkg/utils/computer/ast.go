package computer

import (
	"fmt"
	"math"
)

type Packet interface {
	Version() int
	TypeID() int
	Value() int
	VersionSum() int
}

type meta struct {
	version int
	typeID  int
}

type literal struct {
	meta
	value int
}
type operator struct {
	meta
	lengthTypeID int
	subPackets   []Packet
}

var _ Packet = &literal{}
var _ Packet = &operator{}

func (p *literal) Version() int    { return p.version }
func (p *literal) TypeID() int     { return p.typeID }
func (p *literal) Value() int      { return p.value }
func (p *literal) VersionSum() int { return p.version }

func (p *operator) Version() int { return p.version }
func (p *operator) TypeID() int  { return p.typeID }
func (p *operator) VersionSum() int {
	sum := p.version
	for _, q := range p.subPackets {
		sum += q.VersionSum()
	}
	return sum
}

func (p *operator) Value() int {
	switch p.TypeID() {
	case 0:
		sum := 0
		for _, q := range p.subPackets {
			sum += q.Value()
		}
		return sum
	case 1:
		prod := 1
		for _, q := range p.subPackets {
			prod *= q.Value()
		}
		return prod
	case 2:
		min := math.MaxInt
		for _, q := range p.subPackets {
			v := q.Value()
			if v < min {
				min = v
			}
		}
		return min
	case 3:
		max := math.MinInt
		for _, q := range p.subPackets {
			v := q.Value()
			if v > max {
				max = v
			}
		}
		return max
	case 5:
		a, b := p.subPackets[0].Value(), p.subPackets[1].Value()
		if a > b {
			return 1
		} else {
			return 0
		}
	case 6:
		a, b := p.subPackets[0].Value(), p.subPackets[1].Value()
		if a < b {
			return 1
		} else {
			return 0
		}
	case 7:
		a, b := p.subPackets[0].Value(), p.subPackets[1].Value()
		if a == b {
			return 1
		} else {
			return 0
		}

	default:
		panic(fmt.Sprintf("operator type not implemented: %d", p.TypeID()))
	}
}
