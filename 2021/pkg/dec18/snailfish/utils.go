package snailfish

import "fmt"

func (n *leaf) String() string {
	return fmt.Sprintf("%d", n.value)
}
func (n *pair) String() string {
	return fmt.Sprintf("[%s,%s]", n.left, n.right)
}

func (n *leaf) DeepCopy() Number {
	return Leaf(n.value)
}
func (p *pair) DeepCopy() Number {
	return Pair(p.left.DeepCopy(), p.right.DeepCopy())
}
