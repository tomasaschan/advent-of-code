package dec14

import (
	"fmt"
	"math"
	"strings"
)

// todo:
// - proper recursion
// - proper memoization

func A(input string) int64 {
	return solve(input, 10)
}

func B(input string) int64 {
	return solve(input, 40)
}

func solve(input string, iterations int) int64 {
	template, rules := parse(input)
	expander := polymerExpander{
		rules:    rules,
		elements: map[rune]int64{},
		memo:     map[expansion]count{},
	}
	fmt.Println()

	for i := 1; i < len(template); i++ {
		expander.elements.add(expander.expand(
			expansion{
				pair:       template[i-1 : i+1],
				iterations: iterations,
			}))
	}
	expander.elements[rune(template[len(template)-1])] += 1

	for element, count := range expander.elements {
		fmt.Printf("%c %d\n", element, count)
	}

	min, max := expander.elements.minmax()
	return max - min
}

type polymerExpander struct {
	rules    map[string]string
	memo     map[expansion]count
	elements count
}

type expansion struct {
	pair       string
	iterations int
}

func (e expansion) left(middle string) expansion {
	return expansion{
		pair:       e.pair[:1] + middle,
		iterations: e.iterations - 1,
	}
}
func (e expansion) right(middle string) expansion {
	return expansion{
		pair:       middle + e.pair[1:],
		iterations: e.iterations - 1,
	}
}

type count map[rune]int64

func (a count) add(b count) {
	for r, n := range b {
		a[r] += n
	}
}

func (e polymerExpander) expand(expansion expansion) count {
	if counts, ok := e.memo[expansion]; ok {
		return counts
	}

	if expansion.iterations == 0 {
		counts := map[rune]int64{
			rune(expansion.pair[0]): 1,
		}
		e.memo[expansion] = counts
		return counts
	}

	inserted := e.rules[expansion.pair]

	counts := e.expand(expansion.left(inserted))
	counts.add(e.expand(expansion.right(inserted)))
	e.memo[expansion] = counts
	return counts
}

func (elements count) minmax() (int64, int64) {
	var min, max int64
	min, max = math.MaxInt64, math.MinInt64
	for _, c := range elements {
		if c < min {
			min = c
		}
		if c > max {
			max = c
		}
	}
	return min, max
}

func parse(input string) (template string, rules map[string]string) {
	parts := strings.Split(input, "\n\n")
	template = parts[0]

	rules = map[string]string{}
	for _, line := range strings.Split(parts[1], "\n") {
		if line == "" {
			continue
		}
		parts := strings.Split(line, " -> ")
		rules[parts[0]] = parts[1]
	}

	return
}
