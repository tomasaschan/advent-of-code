package dec4

func A(input string) int {
	announcements, boards, err := parse(input)
	if err != nil {
		panic(err)
	}

	for _, number := range announcements {
		for _, board := range boards {
			board.Announce(number)
			if board.HasBingo() {
				return number * board.Score()
			}
		}
	}

	return 0
}

func B(input string) int {
	announcements, boards, err := parse(input)
	if err != nil {
		panic(err)
	}

	won := map[int]bool{}
	for i := range boards {
		won[i] = false
	}

	for _, number := range announcements {
		for i, board := range boards {
			board.Announce(number)
			if !won[i] && board.HasBingo() {
				won[i] = true

				winners := 0
				for _, w := range won {
					if w {
						winners++
					}
				}
				if winners == len(boards) {
					return board.Score() * number
				}
			}
		}
	}
	return -1
}
