package utils

import (
	"fmt"
	"regexp"
	"strconv"
)

func AsInts(input []string) ([]int, error) {
	result := make([]int, len(input))
	for i, s := range input {
		j, err := strconv.Atoi(s)
		if err != nil {
			return nil, fmt.Errorf("invalid int %s on line %d: %w", s, i, err)
		}
		result[i] = j
	}
	return result, nil
}

func AllInts(line string) []int {
	rx := regexp.MustCompile(`-?\d+`)
	digits := rx.FindAllString(line, -1)
	ints := make([]int, len(digits))

	for i, a := range digits {
		j, err := strconv.Atoi(a)
		if err != nil {
			continue
		}
		ints[i] = j
	}
	return ints
}
