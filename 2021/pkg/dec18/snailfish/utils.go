package snailfish

import "fmt"

func (n *leaf) String() string {
	return fmt.Sprintf("%d", n.value)
}
func (n *pair) String() string {
	return fmt.Sprintf("[%s,%s]", n.left, n.right)
}

func (n *leaf) deepCopy() Number {
	return Leaf(n.value)
}
func (p *pair) deepCopy() Number {
	return Pair(p.left.deepCopy(), p.right.deepCopy())
}

func (l *leaf) equals(n Number) bool {
	if ll, ok := n.(*leaf); ok {
		return ll.value == l.value
	}
	return false
}
func (p *pair) equals(n Number) bool {
	if pp, ok := n.(*pair); ok {
		return pp.left.equals(p.left) && pp.right.equals(p.right)
	}
	return false
}
