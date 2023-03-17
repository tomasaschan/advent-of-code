package collections_test

import (
	"testing"

	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/collections"
)

func TestPriorityQueue(t *testing.T) {
	q := collections.NewPriorityQueue[string, int]()

	q.Push("third", 3)
	q.Push("first", 1)
	q.Push("second", 2)

	v, c := q.Pop()

	if v != "first" {
		t.Fatal("unexpected value popped:", v)
	}
	if c != 1 {
		t.Fatal("unexpected cost popped:", c)
	}
}
