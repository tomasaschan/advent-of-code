package dec8

import (
	"fmt"
	"math"

	"github.com/tomasaschan/advent-of-code-2021/pkg/utils"
)

func A(input string) int {
	count := 0
	for _, test := range parse(input) {
		valueOutputs := test[1]
		for _, output := range valueOutputs {
			switch len(output) {
			case 2:
				// 1
				fallthrough
			case 3:
				// 7
				fallthrough
			case 4:
				// 4
				fallthrough
			case 7:
				// 8
				count++
			case 5:
				// 2, 3, 5
				fallthrough
			case 6:
				// 6, 9, 0
			}
		}
	}

	return count
}

func B(input string) int {
	sum := 0
	for _, display := range parse(input) {
		sum += outputValue(display)
	}

	return sum
}

func outputValue(display [][]string) int {
	fmt.Println()
	mapping := mapWires(append(display[0], display[1]...))

	value := 0
	for i, s := range display[1] {
		value += int(math.Pow(10, float64(3-i))) * mapping.lookup(s)
	}

	return value
}

func mapWires(patterns []string) knowledge {
	k := tabulaRasa()

	// first pass: eliminate impossible combos
	for _, wires := range patterns {
		switch len(wires) {
		case 2:
			k.learn(wires, "cf", 1)
		case 3:
			k.learn(wires, "acf", 7)
		case 4:
			k.learn(wires, "bcdf", 4)
		case 5:
			k.learn(wires, "abcdefg", 2, 3, 5)
		case 6:
			k.learn(wires, "abcdefg", 6, 9, 0)
		case 7:
			k.learn(wires, "abcdefg", 8)
		}
	}
	// after the first pass, 1, 4, 7, and 8 are disambiguated

	// second pass: use more knowledge about all digits

	// out of 2, 3 and 5, only 3 includes all wires from 1
	k.disambiguate(1, 3, true)

	// out of 6, 9 and 0, only 6 does _not_ include all wires from 1
	k.disambiguate(1, 6, false)

	// out of 9 and 0, only 9 includes all wires from 4
	k.disambiguate(4, 9, true)

	// out of 2 and 5, 6 only contains 5
	six := singleton(k.digits[6])
	for candidate := range k.digits[5] {
		if utils.ContainsAll(six, candidate) {
			k.learn(candidate, candidate, 5)
			break
		}
	}

	return k
}
