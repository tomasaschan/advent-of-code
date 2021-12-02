package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

func main() {
	input, err := getInput()
	if err != nil {
		panic(err)
	}
	fmt.Printf("a: %d\nb: %d", a(input), b(input))
}

func a(input []int) int {
	prev := input[0]
	increases := 0

	for _, i := range input[1:] {
		if i > prev {
			increases++
		}
		prev = i
	}

	return increases
}

func b(input []int) int {
	increases := 0

	for i := range input[:len(input)-3] {
		u := input[i]
		v := input[i+1]
		w := input[i+2]
		x := input[i+3]

		if u+v+w < v+w+x {
			increases++
		}
	}
	return increases
}

func getInput() ([]int, error) {
	bytes, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		return nil, err
	}

	result := make([]int, 0)
	for i, line := range strings.Split(string(bytes), "\n") {
		if line == "" {
			continue
		}
		j, err := strconv.Atoi(line)
		if err != nil {
			return nil, fmt.Errorf("error on line %d: %v", i, err)
		}
		result = append(result, int(j))
	}
	return result, nil
}
