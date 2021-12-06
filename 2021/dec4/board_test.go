package dec4

import (
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

func emptyAnnounced() map[int]bool {
	no_announced := map[int]bool{}
	for i := 0; i < 25; i++ {
		no_announced[i] = false
	}
	return no_announced
}

var _ = Describe("Board", func() {
	numbers := map[int]int{
		14: 0, 21: 1, 17: 2, 24: 3, 4: 4,
		10: 5, 16: 6, 15: 7, 9: 8, 19: 9,
		18: 10, 8: 11, 23: 12, 26: 13, 20: 14,
		22: 15, 11: 16, 13: 17, 6: 18, 5: 19,
		2: 20, 0: 21, 12: 22, 3: 23, 7: 24,
	}

	It("no announcements is not bingo", func() {
		board := Board{numbers: numbers, announced: emptyAnnounced()}
		Expect(board.HasBingo()).To(BeFalse())
	})

	It("hard-coded first row is bingo", func() {
		announced := emptyAnnounced()
		announced[0] = true
		announced[1] = true
		announced[2] = true
		announced[3] = true
		announced[4] = true

		board := Board{numbers: numbers, announced: announced}
		Expect(board.HasBingo()).To(BeTrue())
	})

	It("announced bingo is bingo", func() {
		board := Board{numbers: numbers, announced: emptyAnnounced()}
		for _, n := range []int{7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24} {
			board.Announce(n)
		}
		Expect(board.HasBingo()).To(BeTrue())
	})
})
