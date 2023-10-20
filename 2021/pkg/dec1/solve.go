package dec1

import "github.com/tomasaschan/advent-of-code-2021/pkg/utils"

func A(input []string) int {
	measurements, err := utils.AsInts(input)
	if err != nil {
		return -1
	}
	prev := measurements[0]
	increases := 0

	for _, i := range measurements[1:] {
		if i > prev {
			increases++
		}
		prev = i
	}

	return increases
}

func B(input []string) int {
	measurements, err := utils.AsInts(input)
	if err != nil {
		return -1
	}

	increases := 0

	for i := range measurements[:len(measurements)-3] {
		u := measurements[i]
		v := measurements[i+1]
		w := measurements[i+2]
		x := measurements[i+3]

		if u+v+w < v+w+x {
			increases++
		}
	}
	return increases
}
