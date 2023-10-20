package dec21

import (
	"strings"

	"github.com/tomasaschan/advent-of-code-2021/pkg/utils"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/ints"
)

func A(input string) int {
	a, b := parse(input)
	die := deterministicD100{}
	state := deterministicState{die, a, b}

	for {
		state.PlayerATurn()
		if state.IsOver() {
			break
		}
		state.PlayerBTurn()
		if state.IsOver() {
			break
		}
	}

	return state.Result()
}

func B(input string) int {
	a, b := parse(input)
	s := diracState{a, b}

	aWins, bWins := s.countWins()

	return ints.Max(aWins, bWins)
}

func parse(input string) (player, player) {
	lines := strings.Split(strings.TrimSpace(input), "\n")
	apos := utils.AllInts(lines[0])[1]
	bpos := utils.AllInts(lines[1])[1]
	return player{position: apos}, player{position: bpos}
}

type player struct {
	position int
	score    int
}

func (p player) Roll(sum int) player {
	position := ((p.position+sum-1)%10 + 1)
	score := p.score + position
	return player{
		position: position,
		score:    score,
	}
}
