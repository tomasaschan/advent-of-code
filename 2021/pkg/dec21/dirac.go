package dec21

var (
	combos map[int]int = map[int]int{
		3: 1, // 1,1,1
		4: 3, // 1,1,2; 1,2,1; 2,1,1
		5: 6, // 1,1,3; 1,3,1; 3,1,1; 1,2,2; 2,1,2; 2,2,1
		6: 7, // 1,2,3; 1,3,2; 3,1,2; 2,1,3; 2,3,1; 3,2,1; 2,2,2
		7: 6, // 2,2,3; 2,3,2; 3,2,2; 3,3,1; 3,1,3; 1,3,3
		8: 3, // 2,3,3; 3,2,3; 3,3,2
		9: 1, // 3,3,3
	}
)

type diracState struct {
	inTurn player
	other  player
}

func (s diracState) countWins() (int, int) {
	a, b := 0, 0

	for sum, forks := range combos {
		afterPlaying := s.inTurn.Roll(sum)
		if afterPlaying.score >= 21 {
			a += forks
		} else {
			bInc, aInc := diracState{
				inTurn: s.other,
				other:  afterPlaying,
			}.countWins()
			a += aInc * forks
			b += bInc * forks
		}
	}

	return a, b
}
