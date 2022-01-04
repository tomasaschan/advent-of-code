package snailfish

import (
	"fmt"
)

type Number interface {
	fmt.Stringer
	DeepCopy() Number
	explode() Number
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

func (l leaf) explode() Number {
	return &l
}

func (root pair) explode() Number {
	result := root.DeepCopy().(*pair)
	fmt.Println("before exploding", result)

	if p, ok := root.leftmostPairAtDepth(4); ok {
		bomb := root.at(p...).(*pair)
		result.setAt(Leaf(0), p...)
		fmt.Println("after setting 0", result)

		if l, lok := root.neighbor(left, p...); lok {
			result.setAt(
				Leaf(root.at(l...).(*leaf).value+bomb.left.(*leaf).value),
				l...,
			)
			fmt.Println("after setting left", result)
		}
		if r, rok := root.neighbor(right, p...); rok {
			result.setAt(
				Leaf(root.at(r...).(*leaf).value+bomb.right.(*leaf).value),
				r...,
			)
			fmt.Println("after setting right", result)
		}
	}
	fmt.Println("result", result)
	return result
}
