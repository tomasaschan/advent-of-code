package dec4

import (
	"strings"

	"github.com/tomasaschan/advent-of-code-2021/pkg/utils"
)

func parseBoard(input string) (Board, error) {
	lines := strings.Split(input, "\n")
	rows := make([][]int, 0)

	for _, line := range lines {
		if line == "" {
			continue
		}
		rows = append(rows, utils.AllInts(line))
	}

	return NewBoard(rows), nil
}

func parse(input string) ([]int, []Board, error) {
	parts := strings.Split(input, "\n\n")

	announcements := utils.AllInts(parts[0])

	boards := make([]Board, len(parts)-1)
	for i, p := range parts[1:] {
		b, err := parseBoard(p)
		if err != nil {
			return nil, nil, err
		}
		boards[i] = b
	}
	return announcements, boards, nil
}
