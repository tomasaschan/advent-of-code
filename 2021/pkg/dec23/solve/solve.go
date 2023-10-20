package solve

import (
	"fmt"

	"github.com/tomasaschan/advent-of-code-2021/pkg/dec23"
)

func A(input string) {
	state := dec23.Parse(input)
	fmt.Println("Solving a...")
	fmt.Println("a:", astar(state))
}

func B(input string) {
	state := dec23.Parse(input)
	state.PatchB()
	fmt.Println("Solving b...")
	fmt.Println("b:", astar(state))
}
