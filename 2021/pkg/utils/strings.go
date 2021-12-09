package utils

import (
	"sort"
	"strings"
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
