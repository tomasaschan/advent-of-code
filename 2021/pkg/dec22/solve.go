package dec22

import (
	"fmt"
	"strings"

	"github.com/tomasaschan/advent-of-code-2021/pkg/dec22/cuboid"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/ints"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/threed"
)

func A(input string) int {
	instructions, _ := Parse(input)
	domain := cuboid.Cuboid{
		Bottom: threed.Point{X: -50, Y: -50, Z: -50},
		Top:    threed.Point{X: 50, Y: 50, Z: 50},
	}

	return solve(domain, instructions)
}

func B(input string) int {
	instructions, domain := Parse(input)

	return solve(domain, instructions)
}

func solve(domain cuboid.Cuboid, instructions []state) int {
	parts := splitIntoNonOverlapping(domain, instructions)

	count := 0

	for _, part := range parts {
		on := false
		for _, instruction := range instructions {
			if instruction.Cuboid.Contains(part.Bottom) {
				if !instruction.Cuboid.Contains(part.Top) {
					panic(fmt.Sprintf("invalid split! %v only partially covers %v", instruction, part))
				}
				on = instruction.on
			}
		}
		if on {
			count += part.Size()
		}
	}

	return count
}

func splitIntoNonOverlapping(domain cuboid.Cuboid, instructions []state) []cuboid.Cuboid {
	if domain.Size() == 1 {
		return []cuboid.Cuboid{domain}
	}

	for _, instruction := range instructions {
		overlap, overlaps := domain.OverlapWith(instruction.Cuboid)
		if !overlaps {
			continue
		}

		if domain.Bottom != overlap.Bottom || domain.Top != overlap.Top {
			if !domain.Contains(overlap.Bottom) {
				panic(fmt.Sprintf("domain %v does not contain bottom of overlap %v with instruction %v", domain, overlap, instruction.Cuboid))
			}
			parts := []cuboid.Cuboid{}

			for _, subdomain := range domain.Subdivide(overlap.Bottom) {
				if subdomain.Size() == 1 {
					parts = append(parts, subdomain)
				} else if subdomain.Contains(overlap.Top) {
					for _, subpart := range subdomain.Subdivide(overlap.Top) {
						parts = append(parts, splitIntoNonOverlapping(subpart, instructions)...)
					}
				} else {
					parts = append(parts, splitIntoNonOverlapping(subdomain, instructions)...)
				}
			}

			return parts
		}
	}

	return []cuboid.Cuboid{domain}
}

type state struct {
	on bool
	cuboid.Cuboid
}

func (i state) String() (result string) {
	if i.on {
		result = "on:  "
	} else {
		result = "off: "
	}
	result += i.Cuboid.String()

	return
}

func Parse(input string) (instructions []state, domain cuboid.Cuboid) {
	for _, line := range strings.Split(strings.TrimSpace(input), "\n") {
		coords := utils.AllInts(line)

		xlo, xhi, ylo, yhi, zlo, zhi := coords[0], coords[1], coords[2], coords[3], coords[4], coords[5]
		c := cuboid.Cuboid{
			Bottom: threed.Point{X: xlo, Y: ylo, Z: zlo},
			Top:    threed.Point{X: xhi, Y: yhi, Z: zhi},
		}
		instructions = append(instructions, state{
			on:     line[:2] == "on",
			Cuboid: c,
		})
	}

	top, bottom := threed.Point{X: 0, Y: 0, Z: 0}, threed.Point{X: 0, Y: 0, Z: 0}

	for _, i := range instructions {
		t, b := i.Cuboid.Top, i.Cuboid.Bottom
		top.X = ints.Max(top.X, t.X)
		top.Y = ints.Max(top.Y, t.Y)
		top.Z = ints.Max(top.Z, t.Z)

		bottom.X = ints.Min(bottom.X, b.X)
		bottom.Y = ints.Min(bottom.Y, b.Y)
		bottom.Z = ints.Min(bottom.Z, b.Z)
	}

	domain = cuboid.Cuboid{Bottom: bottom, Top: top}

	return
}
