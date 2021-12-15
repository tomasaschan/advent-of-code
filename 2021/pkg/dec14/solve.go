package dec14

import (
	"fmt"
	"math"
	"strings"
)

func A(input string) int {
	return solve(input, 10)
}

func B(input string) int {
	return solve(input, 40)
}

func solve(input string, iterations int) int {
	template, rules := parse(input)
	expander := expander{rules, make(map[expansion]count)}

	counts := make(count)
	for _, r := range template {
		counts[r] += 1
	}

	for i := 1; i < len(template); i++ {
		x := expansion{
			pair:       template[i-1 : i+1],
			iterations: iterations,
		}

		for r, c := range expander.expand(x) {
			counts[r] += c
		}
	}
	fmt.Printf("\nexpanded %s %d times:\n%v\n", template, iterations, counts)
	min, max := counts.minmax()

	return max - min
}

type rules map[string]rune

type count map[rune]int

func (c count) String() string {
	sb := strings.Builder{}
	for r, n := range c {
		sb.WriteString(fmt.Sprintf("\t%c: %d\n", r, n))
	}
	return sb.String()
}

type expansion struct {
	pair       string
	iterations int
}

type expander struct {
	rules rules
	memo  map[expansion]count
}

func (e expander) expand(x expansion) count {
	if c, ok := e.memo[x]; ok {
		return c
	}

	if x.iterations == 0 {
		return make(count)
	}

	inserted := e.rules[x.pair]
	left := expansion{
		pair:       fmt.Sprintf("%c%c", x.pair[0], inserted),
		iterations: x.iterations - 1,
	}
	right := expansion{
		pair:       fmt.Sprintf("%c%c", inserted, x.pair[1]),
		iterations: x.iterations - 1,
	}

	e.memo[x] = make(count)
	e.memo[x][inserted] += 1
	for r, c := range e.expand(left) {
		e.memo[x][r] += c
	}
	for r, c := range e.expand(right) {
		e.memo[x][r] += c
	}
	return e.memo[x]
}

func (c count) minmax() (int, int) {
	min, max := math.MaxInt, math.MinInt
	for _, n := range c {
		if n < min {
			min = n
		}
		if n > max {
			max = n
		}
	}
	return min, max
}

func parse(input string) (template string, rules rules) {
	parts := strings.Split(input, "\n\n")
	template = parts[0]

	rules = map[string]rune{}
	for _, line := range strings.Split(parts[1], "\n") {
		if line == "" {
			continue
		}
		parts := strings.Split(line, " -> ")
		rules[parts[0]] = rune(parts[1][0])
	}

	return
}
