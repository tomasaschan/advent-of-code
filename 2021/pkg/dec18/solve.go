package dec18

import (
	"strings"

	"github.com/tomasaschan/advent-of-code-2021/pkg/dec18/snailfish"
)

func A(input string) int {
	numbers := Parse(input)
	result := Sum(numbers)
	return result.Magnitude()
}

func B(input string) int {
	numbers := Parse(input)

	max := 0
	for _, a := range numbers {
		for _, b := range numbers {
			m := a.Add(b).Magnitude()
			if m > max {
				max = m
			}
		}
	}
	return max
}

func Parse(input string) []snailfish.Number {
	result := make([]snailfish.Number, 0)
	for _, line := range strings.Split(strings.TrimSpace(input), "\n") {
		result = append(result, snailfish.Parse(line))
	}
	return result
}

func Sum(numbers []snailfish.Number) snailfish.Number {
	result := numbers[0]
	for _, n := range numbers[1:] {
		result = result.Add(n)
	}
	return result
}
