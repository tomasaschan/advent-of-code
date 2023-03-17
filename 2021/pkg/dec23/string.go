package dec23

import (
	"strings"

	"github.com/fatih/color"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/twod"
	"golang.org/x/exp/slices"
)

type styler func(string) string

type displayOverrides struct {
	runes      map[twod.Point]rune
	styles     map[twod.Point]styler
	whitespace bool
}

type DisplayOverride func(*displayOverrides)

func withDefaults() DisplayOverride {
	return func(do *displayOverrides) {}
}

func WithOut(x, y int) DisplayOverride {
	return func(do *displayOverrides) {
		do.runes[twod.Point{x, y}] = '.'
	}
}

func With(x, y int, amphipod rune) DisplayOverride {
	return func(do *displayOverrides) { do.runes[twod.Point{x, y}] = amphipod }
}

func Blue(x, y int) DisplayOverride {
	return func(do *displayOverrides) {
		do.styles[twod.Point{x, y}] = func(s string) string { return color.HiBlueString(s) }
	}
}
func NoWhitespace() DisplayOverride {
	return func(do *displayOverrides) { do.whitespace = false }
}

func (s *State) String() string {
	return s.Display(withDefaults())
}

func (s *State) Display(options ...DisplayOverride) string {
	overrides := displayOverrides{
		runes:      make(map[twod.Vector]rune),
		styles:     make(map[twod.Vector]styler),
		whitespace: true,
	}
	for _, option := range options {
		option(&overrides)
	}

	at := func(p twod.Point) string {
		r := s.Amphipods[p]
		if o, ok := overrides.runes[p]; ok {
			r = o
		}
		if r == 0 {
			r = '.'
		}
		s := string(r)
		if style, ok := overrides.styles[p]; ok {
			return style(s)
		}
		return s
	}

	sb := &strings.Builder{}

	for x := range [11]bool{} {
		sb.WriteString(at(twod.Point{x, 0}))
	}
	if overrides.whitespace {
		sb.WriteRune('\n')
	}

	sideroomX := make([]int, 0, 4)
	for _, x := range SideRooms {
		sideroomX = append(sideroomX, x)
	}
	slices.Sort(sideroomX)

	for y := 1; y <= s.Depth; y++ {
		if overrides.whitespace {
			sb.WriteRune(' ')
			sb.WriteRune(' ')
		}
		for _, x := range sideroomX {
			sb.WriteString(at(twod.Point{x, y}))
			if overrides.whitespace {
				sb.WriteRune(' ')
			}
		}
		if overrides.whitespace {
			sb.WriteRune(' ')
			sb.WriteRune('\n')
		}
	}

	return sb.String()
}
