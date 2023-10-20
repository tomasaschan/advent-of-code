package collections

type binaryTree[T any] struct {
	comp func(a T, b T) bool
	data []T
}

func (t *binaryTree[T]) left(n int) int   { return 2*n + 1 }
func (t *binaryTree[T]) right(n int) int  { return 2*n + 2 }
func (t *binaryTree[T]) parent(n int) int { return (n - 1) / 2 }

func (t *binaryTree[T]) balanceDown(n int) {
	left, right := t.left(n), t.right(n)

	if left >= len(t.data) {
		return
	}

	if right >= len(t.data) {
		if t.comp(t.data[left], t.data[n]) {
			t.data[left], t.data[n] = t.data[n], t.data[left]
			t.balanceDown(left)
		}
	} else if t.comp(t.data[left], t.data[right]) {
		if t.comp(t.data[left], t.data[n]) {
			t.data[left], t.data[n] = t.data[n], t.data[left]
			t.balanceDown(left)
		}
	} else {
		if t.comp(t.data[right], t.data[n]) {
			t.data[right], t.data[n] = t.data[n], t.data[right]
			t.balanceDown(right)
		}
	}
}

func (t *binaryTree[T]) balanceUp(n int) {
	if n == 0 {
		return
	}
	parent := t.parent(n)

	if t.comp(t.data[n], t.data[parent]) {
		t.data[n], t.data[parent] = t.data[parent], t.data[n]
		t.balanceUp(parent)
	}
}
