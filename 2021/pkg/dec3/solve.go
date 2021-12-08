package dec3

func A(input []string) int {
	bitCounts := bitCounts(input)

	gammaString := ""
	epsilonString := ""
	for _, b := range bitCounts {
		if b > len(input)/2 {
			gammaString += "1"
			epsilonString += "0"
		} else {
			gammaString += "0"
			epsilonString += "1"
		}
	}

	gamma := readBinary(gammaString)
	epsilon := readBinary(epsilonString)

	return gamma * epsilon
}

func B(input []string) int {
	oxygen := filter(input, 0, func(ones int, zeroes int) bool {
		return zeroes <= ones
	})
	co2 := filter(input, 0, func(ones int, zeroes int) bool {
		return ones < zeroes
	})
	return oxygen * co2
}

func bitCounts(input []string) []int {
	state := make([]int, len(input[0]))
	for i := range state {
		state[i] = 0
	}

	for _, row := range input {
		for i, c := range row {
			if c == '1' {
				state[i] += 1
			}
		}
	}

	return state
}

func filter(input []string, from int, wantOne func(int, int) bool) int {
	if len(input) == 1 {
		return readBinary(input[0])
	}

	bitCounts := bitCounts(input)

	survivors := make([]string, 0)
	targetsOne := wantOne(bitCounts[from], len(input)-bitCounts[from])

	for _, line := range input {
		if (rune(line[from]) == '1') == targetsOne {
			survivors = append(survivors, line)
		}
	}

	return filter(survivors, from+1, wantOne)
}

func readBinary(binary string) int {
	decimal := 0
	for i, r := range binary {
		k := len(binary) - 1 - i
		if r == '1' {
			decimal += 1 << k
		}
	}
	return decimal
}
