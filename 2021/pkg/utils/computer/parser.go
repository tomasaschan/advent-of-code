package computer

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
)

var (
	ErrNotEnoughInput error = errors.New("not enough input")
)

type Parser interface {
	Has(n int) bool
	Peek(n int) (string, error)
	Consume(n int) (string, error)
	IsEOF() bool
}

type parser struct {
	input string
	pos   int
}

func NewParserForHex(input string) (*parser, error) {
	binInput := strings.Builder{}

	for _, r := range input {
		i, err := strconv.ParseInt(string(r), 16, 8)
		if err != nil {
			return nil, err
		}
		s := fmt.Sprintf("%04b", i)
		binInput.WriteString(s)
	}

	return NewParserForBin(binInput.String()), nil
}

func NewParserForBin(input string) *parser {
	return &parser{
		input: input,
		pos:   0,
	}
}

func (p *parser) Peek(n int) (string, error) {
	if p.pos+n > len(p.input) {
		return "", ErrNotEnoughInput
	}
	return p.input[p.pos : p.pos+n], nil
}
func (p *parser) Consume(n int) (string, error) {
	s, err := p.Peek(n)
	if err != nil {
		return "", err
	}
	p.pos += n
	return s, nil

}
func (p *parser) Has(n int) bool {
	return p.pos+n <= len(p.input)
}
func (p *parser) IsEOF() bool {
	return p.pos == len(p.input)
}
