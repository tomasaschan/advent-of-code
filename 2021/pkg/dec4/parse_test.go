package dec4

import (
	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"
)

var _ = Describe("Dec 4 parsing", func() {
	input := `7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
8  2 23  4 24
21  9 14 16  7
6 10  3 18  5
1 12 20 15 19

3 15  0  2 22
9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
2  0 12  3  7
`
	It("can parse input", func() {
		announcements, boards, err := parse(input)

		Expect(err).NotTo(HaveOccurred())
		Expect(announcements).To(Equal(
			[]int{7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1}),
			"announcements",
		)

		announced := map[int]bool{}
		for i := 0; i < 25; i++ {
			announced[i] = false
		}

		Expect(boards[0:3]).To(
			Equal([]Board{
				{
					numbers: map[int]int{
						22: 0, 13: 1, 17: 2, 11: 3, 0: 4,
						8: 5, 2: 6, 23: 7, 4: 8, 24: 9,
						21: 10, 9: 11, 14: 12, 16: 13, 7: 14,
						6: 15, 10: 16, 3: 17, 18: 18, 5: 19,
						1: 20, 12: 21, 20: 22, 15: 23, 19: 24,
					},
					announced: announced,
				},
				{
					numbers: map[int]int{
						3: 0, 15: 1, 0: 2, 2: 3, 22: 4,
						9: 5, 18: 6, 13: 7, 17: 8, 5: 9,
						19: 10, 8: 11, 7: 12, 25: 13, 23: 14,
						20: 15, 11: 16, 10: 17, 24: 18, 4: 19,
						14: 20, 21: 21, 16: 22, 12: 23, 6: 24,
					},
					announced: announced,
				},
				{
					numbers: map[int]int{
						14: 0, 21: 1, 17: 2, 24: 3, 4: 4,
						10: 5, 16: 6, 15: 7, 9: 8, 19: 9,
						18: 10, 8: 11, 23: 12, 26: 13, 20: 14,
						22: 15, 11: 16, 13: 17, 6: 18, 5: 19,
						2: 20, 0: 21, 12: 22, 3: 23, 7: 24,
					},
					announced: announced,
				},
			}),
			"boards",
		)
	})
})
