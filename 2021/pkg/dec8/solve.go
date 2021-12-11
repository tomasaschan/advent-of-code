package dec8

import (
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
	wiring := mapWires(append(display[0], display[1]...))

	value := 0
	for i, s := range display[1] {
		value += int(math.Pow(10, float64(3-i))) * wiring[s]
	}

	return value
}

func mapWires(patterns []string) map[string]int {
	var (
		one, two, three, four, five, six, seven, eight, nine, zero string
		twos, threes, fives, sixes, nines, zeroes                  map[string]bool = map[string]bool{}, map[string]bool{}, map[string]bool{}, map[string]bool{}, map[string]bool{}, map[string]bool{}
	)

	// first pass: group wire patterns by number of wires
	for _, pattern := range patterns {
		switch len(pattern) {
		case 2:
			one = pattern
		case 3:
			seven = pattern
		case 4:
			four = pattern
		case 5:
			twos[pattern] = true
			threes[pattern] = true
			fives[pattern] = true
		case 6:
			sixes[pattern] = true
			nines[pattern] = true
			zeroes[pattern] = true
		case 7:
			eight = pattern
		}
	}
	// out of 6, 9 and 0, only 9 covers 4
	for p := range sixes {
		if utils.ContainsAll(p, four) {
			nine = p
			delete(sixes, p)
			delete(zeroes, p)
		}
	}

	// out of 6, and 0, only 0 covers 1
	for p := range zeroes {
		if utils.ContainsAll(p, one) {
			zero = p
			delete(sixes, p)
		}
	}

	// now, 6 has only one option
	for p := range sixes {
		six = p
	}

	// out of 2, 3 and 5, only 3 covers 1
	for p := range threes {
		if utils.ContainsAll(p, one) {
			three = p
			delete(twos, p)
			delete(fives, p)
		}
	}

	// out of 2 and 5, only 5 is covered by 6
	for p := range fives {
		if utils.ContainsAll(six, p) {
			five = p
			delete(twos, p)
		}
	}

	// now, 2 only has one option
	for p := range twos {
		two = p
	}

	return map[string]int{
		zero:  0,
		one:   1,
		two:   2,
		three: 3,
		four:  4,
		five:  5,
		six:   6,
		seven: 7,
		eight: 8,
		nine:  9,
	}
}
