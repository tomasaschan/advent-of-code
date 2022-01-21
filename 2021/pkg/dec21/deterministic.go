package dec21

import (
	"fmt"
)

type deterministicD100 struct {
	current int
}

func (d deterministicD100) String() string { return fmt.Sprintf("deterministic D100 @ %d", d.current) }
func (d deterministicD100) RollCount() int { return d.current }
func (d deterministicD100) Roll() (int, deterministicD100) {
	return d.current%100 + 1, deterministicD100{current: d.current + 1}
}
func (d deterministicD100) RollThrice() (int, deterministicD100) {
	x, die := d.Roll()
	y, die := die.Roll()
	z, die := die.Roll()
	return x + y + z, die
}

type deterministicState struct {
	die deterministicD100
	a   player
	b   player
}

func (s *deterministicState) IsOver() bool {
	return s.a.score >= 1000 || s.b.score >= 1000
}

func (s *deterministicState) PlayerATurn() {
	sum, die := s.die.RollThrice()
	s.die = die
	s.a = s.a.Roll(sum)
}
func (s *deterministicState) PlayerBTurn() {
	sum, die := s.die.RollThrice()
	s.die = die
	s.b = s.b.Roll(sum)
}

func (s *deterministicState) Result() int {
	var losingScore int
	if s.a.score >= 1000 {
		losingScore = s.b.score
	} else if s.b.score >= 1000 {
		losingScore = s.a.score
	} else {
		panic("tried to get result of unfinished game")
	}

	return losingScore * s.die.RollCount()
}
