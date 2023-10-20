package collections

type Set[T comparable] interface {
	Has(T) bool
	Add(T)
	Remove(T)
	Empty() bool
	Iter() <-chan T
	Size() int
}

type set[T comparable] map[T]bool

var _ Set[int] = &set[int]{}

func EmptySet[T comparable]() Set[T] {
	return &set[T]{}
}

func (s *set[T]) Has(x T) bool {
	_, ok := (*s)[x]
	return ok
}

func (s *set[T]) Add(x T) {
	(*s)[x] = true
}

func (s *set[T]) Remove(x T) {
	delete(*s, x)
}

func (s *set[T]) Size() int {
	return len(*s)
}

func (s *set[T]) Empty() bool {
	return len(*s) != 0
}

func (s *set[T]) Iter() <-chan T {
	c := make(chan T)

	go func() {
		for x := range *s {
			c <- x
		}
		close(c)
	}()

	return c
}
