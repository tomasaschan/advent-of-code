package dec2

import (
	"regexp"
	"strconv"
)

func A(input []string) int {
	plan := mustParse(input)
	x, d := 0, 0

	for _, i := range plan {
		switch i.command {
		case "forward":
			x += i.argument
		case "up":
			d -= i.argument
		case "down":
			d += i.argument
		}
	}

	return x * d
}

func B(input []string) int {
	plan := mustParse(input)
	x, d, aim := 0, 0, 0

	for _, i := range plan {
		switch i.command {
		case "up":
			aim -= i.argument
		case "down":
			aim += i.argument
		case "forward":
			x += i.argument
			d += i.argument * aim
		}
	}

	return x * d
}

type instruction struct {
	command  string
	argument int
}

func mustParse(input []string) []instruction {
	rx := regexp.MustCompile(`(\w+) (\d+)`)

	result := make([]instruction, 0)
	for _, line := range input {
		if line == "" {
			continue
		}
		parts := rx.FindSubmatch([]byte(line))
		arg, err := strconv.Atoi(string(parts[2]))
		if err != nil {
			panic(err)
		}
		result = append(result, instruction{
			command:  string(parts[1]),
			argument: arg,
		})
	}

	return result
}
