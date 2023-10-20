package snailfish

import (
	"fmt"
)

type Number interface {
	fmt.Stringer
	Add(n Number) Number
	Magnitude() int
	deepCopy() Number
	reduce() Number
	explode() Number
	split() Number
	equals(n Number) bool
	navigatable
}

type side string

const (
	left  side = "l"
	right side = "r"
)

func (s side) other() side {
	switch s {
	case left:
		return right
	case right:
		return left
	default:
		panic(fmt.Sprintf("other side of %v could not be determined", s))
	}
}

type pair struct {
	left  Number
	right Number
}

func Pair(left Number, right Number) Number { return &pair{left: left, right: right} }

type leaf struct{ value int }

func Leaf(value int) Number { return &leaf{value: value} }

func (l leaf) reduce() Number { return &l }

func (l *leaf) Add(n Number) Number {
	result := Pair(l, n).reduce()
	return result
}
func (p *pair) Add(n Number) Number {
	result := Pair(p, n).reduce()
	return result
}

func (l *leaf) Magnitude() int { return l.value }
func (p *pair) Magnitude() int { return 3*p.left.Magnitude() + 2*p.right.Magnitude() }

func (root *pair) reduce() Number {
	var result, tmp Number = root, root
	for {
		result, tmp = result.explode(), result
		if !result.equals(tmp) {
			continue
		}
		result, tmp = result.split(), result
		if !result.equals(tmp) {
			continue
		}

		break
	}
	return result
}

func (l leaf) explode() Number { return &l }

func (root pair) explode() Number {
	result := root.deepCopy().(*pair)

	if p, ok := root.leftmostPairAtDepth(4); ok {
		bomb := root.at(p...).(*pair)
		result.setAt(Leaf(0), p...)

		if l, lok := root.neighbor(left, p...); lok {
			result.setAt(
				Leaf(root.at(l...).(*leaf).value+bomb.left.(*leaf).value),
				l...,
			)
		}
		if r, rok := root.neighbor(right, p...); rok {
			_, aok := root.at(r...).(*leaf)
			_, bok := bomb.right.(*leaf)
			if !aok || !bok {
				fmt.Printf("expected %v to be a pair of two leaves at %v in %v\n", bomb, p, root.String())
				fmt.Printf("expected %v to be a neighboring leaf at %v in %v\n", root.at(r...), r, root.String())
			}
			result.setAt(
				Leaf(root.at(r...).(*leaf).value+bomb.right.(*leaf).value),
				r...,
			)
		}
	}
	return result
}

func (n leaf) split() Number {
	if n.value%2 == 0 {
		return Pair(Leaf(n.value/2), Leaf(n.value/2))
	} else {
		return Pair(Leaf(n.value/2), Leaf(n.value/2+1))
	}
}

func (root pair) split() Number {
	result := root.deepCopy().(*pair)

	if p, ok := root.leftmostGreaterThan(10); ok {
		result.setAt(result.at(p...).split(), p...)
	}
	return result
}
