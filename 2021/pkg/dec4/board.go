package dec4

type Board struct {
	numbers   map[int]int  // n => 5 * row + col
	announced map[int]bool // 5*row + col => bool
}

func NewBoard(numbers [][]int) Board {
	b := Board{
		numbers:   map[int]int{},
		announced: map[int]bool{},
	}
	for r, row := range numbers {
		for c, n := range row {
			b.numbers[n] = at(r, c)
			b.announced[at(r, c)] = false
		}
	}

	return b
}

func at(row int, col int) int {
	return 5*row + col
}

func (b *Board) Announce(number int) {
	pos, ok := b.numbers[number]
	if !ok {
		return
	}
	b.announced[pos] = true
}

func (b *Board) HasBingo() bool {
	for i := 0; i < 5; i++ {
		// check rows
		r := true
		for c := 0; c < 5; c++ {
			r = r && b.announced[at(i, c)]
			if !r {
				break
			}
		}
		if r {
			return true
		}

		// check columns
		c := true
		for r := 0; r < 5; r++ {
			c = c && b.announced[at(r, i)]
			if !c {
				break
			}
		}
		if c {
			return true
		}
	}

	return false
}

func (b *Board) Score() int {
	sum := 0
	for n, pos := range b.numbers {
		if !b.announced[pos] {
			sum += n
		}
	}
	return sum
}
