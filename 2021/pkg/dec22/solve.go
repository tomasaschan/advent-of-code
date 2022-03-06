package dec22

import (
	"fmt"
	"strings"

	"github.com/tomasaschan/advent-of-code-2021/pkg/dec22/cuboid"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/threed"
)

func A(input string) int {
	fmt.Println(parse(input))

	return 1
}

func B(input string) int {
	return 0
}

type instruction struct {
	on bool
	cuboid.Cuboid
}

func parse(input string) []instruction {
	result := make([]instruction, 0)

	for _, line := range strings.Split(strings.TrimSpace(input), "\n") {
		coords := utils.AllInts(line)

		xlo, xhi, ylo, yhi, zlo, zhi := coords[0], coords[1], coords[2], coords[3], coords[4], coords[5]
		c, _ := cuboid.NewCuboid(&threed.Point{X: xlo, Y: ylo, Z: zlo},
			&threed.Point{X: xhi, Y: yhi, Z: zhi},
		)
		result = append(result, instruction{
			on:     line[:2] == "on",
			Cuboid: c,
		})
	}

	return result
}
