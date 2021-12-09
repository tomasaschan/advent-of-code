package dec8

import (
	"strings"

	"github.com/tomasaschan/advent-of-code-2021/pkg/utils"
)

func parse(input string) [][][]string {
	tests := strings.Split(input, "\n")

	results := [][][]string{}
	for _, test := range tests {
		if test == "" {
			continue
		}
		parts := strings.Split(test, "|")
		testSignals := strings.Split(strings.TrimSpace(parts[0]), " ")
		valueOutputs := strings.Split(strings.TrimSpace(parts[1]), " ")

		tests := []string{}
		for _, s := range testSignals {
			tests = append(tests, utils.StringSorted(s))
		}
		values := []string{}
		for _, s := range valueOutputs {
			values = append(values, utils.StringSorted(s))
		}
		results = append(results, [][]string{tests, values})
	}

	return results
}
