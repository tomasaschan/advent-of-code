package collections

import (
	"container/heap"

	"golang.org/x/exp/constraints"
)

type PriorityQueue[T any, P constraints.Ordered] interface {
	Empty() bool
	Len() int
	Push(x T, cost P)
	Pop() (T, P)
}

type item[T any, P constraints.Ordered] struct {
	data T
	cost P
}

type pq[T any, P constraints.Ordered] struct {
	data *qdata[T, P]
}

type qdata[T any, P constraints.Ordered] []*item[T, P]

func (d qdata[T, P]) Less(i, j int) bool {
	return d[i].cost < d[j].cost
}

func (d *qdata[T, P]) Pop() any {
	old := *d

	n := len(old)
	item := old[n-1]
	old[n-1] = nil // avoid memory leak
	*d = old[0 : n-1]
	return item
}

func (d *qdata[T, P]) Push(x any) {
	item := x.(*item[T, P])
	*d = append(*d, item)
}

func (d *qdata[T, P]) Swap(i, j int) {
	(*d)[i], (*d)[j] = (*d)[j], (*d)[i]
}

func (d *qdata[T, P]) Len() int { return len(*d) }

var _ PriorityQueue[interface{}, int] = &pq[interface{}, int]{}
var _ heap.Interface = &qdata[interface{}, int]{}

func NewPriorityQueue[T any, P constraints.Ordered]() PriorityQueue[T, P] {
	return &pq[T, P]{data: &qdata[T, P]{}}
}

func (q *pq[T, P]) Empty() bool {
	return q.Len() == 0
}

func (q *pq[T, P]) Len() int {
	return q.data.Len()
}

func (q *pq[T, P]) Push(x T, cost P) {
	heap.Push(q.data, &item[T, P]{data: x, cost: cost})
}

func (q *pq[T, P]) Pop() (T, P) {
	item := heap.Pop(q.data).(*item[T, P])
	return item.data, item.cost
}
