package utils

import (
	"fmt"
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
