package dec8

import (
	"fmt"
	"regexp"
	"sort"
	"strings"

	"github.com/tomasaschan/advent-of-code-2021/pkg/utils"
)

type knowledge struct {
	wirings map[rune]string
	digits  map[int]map[string]bool
}

func tabulaRasa() knowledge {
	k := knowledge{
		wirings: map[rune]string{},
		digits:  map[int]map[string]bool{},
	}
	wires := "abcdefg"
	for _, r := range wires {
		k.wirings[r] = wires
	}
	for i := range [10]int{} {
		k.digits[i] = map[string]bool{}
	}
	return k
}

func (k knowledge) learn(wires string, targets string, digits ...int) {
	matching := regexp.MustCompile("[" + targets + "]")
	others := regexp.MustCompile("[^" + targets + "]")

	for r := range k.wirings {
		if strings.ContainsRune(wires, r) && matching.MatchString(k.wirings[r]) {
			k.wirings[r] = others.ReplaceAllString(k.wirings[r], "")
		} else if others.MatchString(k.wirings[r]) {
			k.wirings[r] = matching.ReplaceAllString(k.wirings[r], "")
		}
	}
	if wires == targets && len(digits) == 1 {
		// we have enough info to disambiguate;
		// set the digit to the candidate, and remove candidate from all other digits
		for d := range k.digits {
			if d == digits[0] {
				k.digits[d] = map[string]bool{wires: true}
			} else {
				delete(k.digits[d], wires)
			}
		}
	} else {
		// we don't have enough to disambiguate;
		// just register all digits these wires could represent
		for _, option := range digits {
			k.digits[option][wires] = true
		}
	}
}

func (k knowledge) disambiguate(small int, large int, contains bool) {
	needle := singleton(k.digits[small])
	for candidate := range k.digits[large] {
		if utils.ContainsAll(candidate, needle) == contains {
			k.learn(candidate, candidate, large)
			break
		}
	}
}

func (k knowledge) lookup(wires string) int {
	for i := range [10]int{} {
		if singleton(k.digits[i]) == wires {
			return i
		}
	}
	return -1
}

func (k knowledge) String() string {
	sb := strings.Builder{}
	for _, r := range "abcdefg" {
		sb.WriteRune(r)
		sb.WriteString(": ")
		sb.WriteString(k.wirings[r])
		sb.WriteRune('\n')
	}
	sb.WriteRune('\n')

	for i := range [10]int{} {
		options := sort.StringSlice([]string{})
		for o := range k.digits[i] {
			options = append(options, o)
		}
		sort.Sort(options)

		sb.WriteString(fmt.Sprintf("%d: %s\n", i, strings.Join(options, ", ")))
	}
	return sb.String()
}

func singleton(options map[string]bool) string {
	if len(options) != 1 {
		panic(fmt.Sprintf("tried to get singleton value from %v", options))
	}
	for v := range options {
		return v
	}
	panic("unreachable")
}
