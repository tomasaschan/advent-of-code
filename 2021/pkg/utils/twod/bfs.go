package twod

import "container/heap"

type pqItem struct {
	x     interface{}
	cost  int
	index int
}

type pq []*pqItem

var _ heap.Interface = &pq{}

func (q pq) Len() int {
	return len(q)
}

func (q pq) Less(i, j int) bool {
	return q[i].cost < q[j].cost
}

func (q *pq) Pop() interface{} {
	old := *q
	n := len(old)
	if n == 0 {
		return nil
	}
	item := old[n-1]
	old[n-1] = nil  // avoid mem leak
	item.index = -1 // for safety
	*q = old[0 : n-1]
	return item
}

func (q *pq) Push(x interface{}) {
	n := len(*q)
	item := x.(pqItem)
	item.index = n
	*q = append(*q, &item)
}

func (q pq) Swap(i, j int) {
	q[i], q[j] = q[j], q[i]
	q[i].index, q[j].index = i, j
}

type priorityQueue struct {
	q *pq
}

func PriorityQueue() priorityQueue {
	return priorityQueue{q: &pq{}}
}

func (q priorityQueue) Push(v Vector, cost int) {
	heap.Push(q.q, pqItem{x: &v, cost: cost})
}
func (q priorityQueue) Pop() (*Vector, int) {
	if q.q.Len() == 0 {
		return nil, 0
	}

	item := heap.Pop(q.q)
	pqi := item.(*pqItem)
	return pqi.x.(*Vector), pqi.cost
}
func (q priorityQueue) Len() int {
	return q.q.Len()
}
