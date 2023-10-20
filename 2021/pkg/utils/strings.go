package utils

import (
	"fmt"
	"regexp"
	"sort"
	"strings"

	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/ints"
)

type sortRuneString []rune

func (s sortRuneString) Len() int {
	return len(s)
}
func (s sortRuneString) Less(i int, j int) bool {
	return s[i] < s[j]
}
func (s sortRuneString) Swap(i int, j int) {
	s[i], s[j] = s[j], s[i]
}

func StringSorted(s string) string {
	runes := []rune(s)
	sort.Sort(sortRuneString(runes))
	return string(runes)
}

func ContainsAll(haystack string, needle string) bool {
	for _, r := range needle {
		if !strings.ContainsRune(haystack, r) {
			return false
		}
	}
	return true
}

func Columnar(inputs ...fmt.Stringer) string {
	cells := make([][]string, len(inputs))

	for col, column := range inputs {
		lines := strings.Split(strings.Trim(column.String(), "\n"), "\n")
		cells[col] = make([]string, len(lines))
		copy(cells[col], lines)
	}

	lines, widths := 0, make([]int, len(inputs))

	stripRx := regexp.MustCompile("[\u001b\u009b][[()#;?]*(?:[0-9]{1,4}(?:;[0-9]{0,4})*)?[0-9A-ORZcf-nqry=><]")
	for col, column := range cells {
		lines = ints.Max(lines, len(column))
		for _, cell := range column {
			widths[col] = ints.Max(widths[col], len(stripRx.ReplaceAllString(cell, "")))
		}
	}

	sb := &strings.Builder{}

	for row := 0; row < lines; row++ {
		for col := 0; col < len(inputs); col++ {
			if row < len(cells[col]) {
				leftMargin := widths[col] - len(stripRx.ReplaceAllString(cells[col][row], ""))
				if leftMargin > 0 {
					sb.WriteString(strings.Repeat(" ", leftMargin))
				}
				sb.WriteString(cells[col][row])
			} else {
				sb.WriteString(strings.Repeat(" ", widths[col]))
			}
			if col != len(inputs)-1 {
				sb.WriteString("  ")
			} else {
				sb.WriteString("\n")
			}
		}
	}

	return sb.String()
}
