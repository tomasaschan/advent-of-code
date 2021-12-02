package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func main() {
	input, err := read()
	if err != nil {
		panic(err)
	}
	fmt.Printf("a: %d\nb: %d\n", a(input), b(input))
}

func a(plan []instruction) int {
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

func b(plan []instruction) int {
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

func read() ([]instruction, error) {
	bytes, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		return nil, err
	}
	rx := regexp.MustCompile(`(\w+) (\d+)`)

	result := make([]instruction, 0)
	for _, line := range strings.Split(string(bytes), "\n") {
		if line == "" {
			continue
		}
		parts := rx.FindSubmatch([]byte(line))
		arg, err := strconv.Atoi(string(parts[2]))
		if err != nil {
			return nil, err
		}
		result = append(result, instruction{
			command:  string(parts[1]),
			argument: arg,
		})
	}

	return result, nil
}
