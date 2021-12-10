package dec10

import (
	"container/list"
	"sort"
	"strings"
)

func A(input string) int {
	score := 0
	for _, line := range strings.Split(input, "\n") {
		s, _ := scoreLine(line)
		score += s
	}

	return score
}

func B(input string) int {
	scores := make([]int, 0)
	for _, line := range strings.Split(input, "\n") {
		_, s := scoreLine(line)
		if s != 0 {
			scores = append(scores, s)
		}
	}

	sort.Slice(scores, func(i, j int) bool { return scores[i] < scores[j] })
	return scores[len(scores)/2]
}

var (
	errorScores map[rune]int = map[rune]int{
		')': 3,
		']': 57,
		'}': 1197,
		'>': 25137,
	}
	openers = "([{<"
	closers = ")]}>"
)

func scoreLine(line string) (int, int) {
	stack := list.List{}

	for _, r := range line {
		if strings.ContainsRune(openers, r) {
			stack.PushBack(r)
		} else if strings.ContainsRune(closers, r) {
			expected := rune(openers[strings.IndexRune(closers, r)])
			back := stack.Back()
			if expected == back.Value {
				stack.Remove(back)
			} else {
				return errorScores[r], 0
			}
		}
	}

	completionScore := 0
	for e := stack.Back(); e != nil; e = e.Prev() {
		completionScore = completionScore*5 + strings.IndexRune(openers, e.Value.(rune)) + 1
	}
	return 0, completionScore
}
