package snailfish

import "fmt"

type navigatable interface {
	at(path ...side) Number
}

func (n leaf) at(path ...side) Number {
	if len(path) > 0 {
		panic(fmt.Sprintf("tried to descend into number: %v", path))
	}
	return &n
}

func (root *pair) at(path ...side) Number {
	if len(path) == 0 {
		return root
	}
	if path[len(path)-1] == left {
		return root.left.at(path[:len(path)-1]...)
	}
	if path[len(path)-1] == right {
		return root.right.at(path[:len(path)-1]...)
	}
	panic(fmt.Sprintf("could not get number at %v from %v", path, root))
}

func (root *pair) setAt(n Number, path ...side) {
	if p, ok := root.at(path[1:]...).(*pair); ok {
		switch path[0] {
		case left:
			p.left = n
		case right:
			p.right = n
		}
	}
}

func (root *pair) neighbor(dir side, path ...side) ([]side, bool) {
	if len(path) == 0 {
		// neightbor of root is undefined
		return nil, false
	} else if path[0] == dir.other() {
		// neighbor opposite of a pair is found by going as close
		// as possible starting on the other half of the pair
		return root.furthest(dir.other(), append([]side{dir}, path[1:]...)...)
	} else {
		// recurse
		return root.neighbor(dir, path[1:]...)
	}
}

func (root *pair) furthest(dir side, path ...side) ([]side, bool) {
	n := root.at(path...)

	if _, ok := n.(*pair); ok {
		return root.furthest(dir, append([]side{dir}, path...)...)
	} else if _, ok := n.(*leaf); ok {
		return path, true
	} else {
		return nil, false
	}
}

func (root *pair) leftmostPairAtDepth(d int) ([]side, bool) {
	if d == 0 {
		return []side{}, true
	}
	if p, ok := root.left.(*pair); ok {
		if rest, ok := p.leftmostPairAtDepth(d - 1); ok {
			return append(rest, left), true
		}
	}
	if p, ok := root.right.(*pair); ok {
		if rest, ok := p.leftmostPairAtDepth(d - 1); ok {
			return append(rest, right), true
		}
	}
	return nil, false
}

func (p *pair) leftmostGreaterThan(d int) ([]side, bool) {
	if l, ok := p.left.(*leaf); ok && l.value >= 10 {
		return []side{left}, true
	}
	if l, ok := p.left.(*pair); ok {
		if p, ok := l.leftmostGreaterThan(d); ok {
			return append(p, left), true
		}
	}
	if r, ok := p.right.(*leaf); ok && r.value >= 10 {
		return []side{right}, true
	}
	if r, ok := p.right.(*pair); ok {
		if p, ok := r.leftmostGreaterThan(d); ok {
			return append(p, right), true
		}
	}
	return nil, false
}
