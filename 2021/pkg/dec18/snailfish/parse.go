package snailfish

import (
	"strconv"
	"strings"
)

func parse1(input string) Number {
	n, rest := read1(input)

	if rest != "" {
		panic("parsing number didn't consume the entire string; leftovers: " + rest)
	}
	return *n
}

func read1(input string) (*Number, string) {
	if input == "" {
		return nil, ""
	}

	if input[0] == '[' {
		left, rest := read1(input[1:]) // drop the [; read next number
		right, rest := read1(rest[1:]) // drop the ,; read next number
		n := Pair(*left, *right)
		return &n, rest[1:] // drop the ]
	} else {
		digits := ""
		rest := input
		for rest = input; strings.ContainsRune("0123456789", rune(rest[0])); rest = rest[1:] {
			digits += string(rest[0])
		}
		value, err := strconv.Atoi(digits)
		if err != nil {
			panic(err)
		}
		n := Leaf(value)
		return &n, rest
	}
}
