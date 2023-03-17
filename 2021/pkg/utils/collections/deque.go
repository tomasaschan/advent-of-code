package collections

type Deque[T any] interface {
	Push(x T)
	Pop() (T, bool)
	Len() int
	Empty() bool
	Iter() <-chan T
}

type deque[T any] struct {
	data []T
}

var _ Deque[int] = &deque[int]{}

func NewDeque[T any]() Deque[T] {
	return &deque[T]{data: make([]T, 0)}
}

func (d *deque[T]) Push(x T) {
	d.data = append(d.data, x)
}

func (d *deque[T]) Pop() (T, bool) {
	var zero T

	if len(d.data) == 0 {
		return zero, false
	}

	value := d.data[0]
	d.data[0] = zero // avoid memory leak
	d.data = d.data[1:]

	return value, true
}

func (d *deque[T]) Len() int {
	return len(d.data)
}

func (d *deque[T]) Empty() bool {
	return len(d.data) == 0
}

func (d *deque[T]) Iter() <-chan T {
	ch := make(chan T)
	go func() {
		for _, x := range d.data {
			ch <- x
		}
		close(ch)
	}()
	return ch
}
