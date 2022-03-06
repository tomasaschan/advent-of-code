package cuboid

import (
	"fmt"

	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/ints"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/threed"
)

type cuboid struct {
	Bottom *threed.Point
	Top    *threed.Point
}
type Cuboid interface {
	Size() int
	FindOverlap(Cuboid) (CuboidSet, CuboidSet, CuboidSet)
	fmt.Stringer
}

func NewCuboid(bottom, top *threed.Point) (Cuboid, bool) {
	if bottom.X <= top.X && bottom.Y <= top.Y && bottom.Z <= top.Z {
		return &cuboid{Bottom: bottom, Top: top}, true
	}
	return nil, false
}

func (c *cuboid) Size() int {
	return (1 + c.Top.X - c.Bottom.X) * (1 + c.Top.Y - c.Bottom.Y) * (1 + c.Top.Z - c.Top.Z)
}

func (c *cuboid) String() string {
	return fmt.Sprintf("Cuboid({%d,%d,%d}, {%d,%d,%d})", c.Bottom.X, c.Bottom.Y, c.Bottom.Z, c.Top.X, c.Top.Y, c.Top.Z)
}

// FindOverlap returns three lists of cuboids, where the elements represent, in order:
//
// 1. Cuboids representing the part of space that is only in a
//
// 2. Cuboids representing the part of space that is in both a and b
//
// 3. Cuboids representing the part of space that is only in b
func (a *cuboid) FindOverlap(b Cuboid) (CuboidSet, CuboidSet, CuboidSet) {
	fmt.Println("finding non-overlapping parts of", a.String(), "and", b.String())
	onlyA, both, onlyB := NewSet(), NewSet(), NewSet()

	symmetrically(leftOf, a, b.(*cuboid), onlyA, onlyB)
	symmetrically(rightOf, a, b.(*cuboid), onlyA, onlyB)

	symmetrically(behind, a, b.(*cuboid), onlyA, onlyB)
	symmetrically(inFrontOf, a, b.(*cuboid), onlyA, onlyB)

	symmetrically(below, a, b.(*cuboid), onlyA, onlyB)
	symmetrically(above, a, b.(*cuboid), onlyA, onlyB)

	if c, ok := overlap(a, b.(*cuboid)); ok {
		both.Add(c)
	}

	fmt.Println("only in a:", onlyA)
	fmt.Println("only in b:", onlyB)
	fmt.Println("overlap:", both)

	return onlyA, both, onlyB
}

func symmetrically(f func(a, b *cuboid) (Cuboid, bool), a, b *cuboid, onlyA, onlyB CuboidSet) {
	if x, ok := f(a, b); ok {
		onlyA.Add(x)
	}
	if x, ok := f(b, a); ok {
		onlyB.Add(x)
	}
}

// left/right of is along the -/+ x-axis, and extends infinitely in y and z

func leftOf(a, b *cuboid) (Cuboid, bool) {
	fmt.Println("\tfinding part of", a.String(), "which is left of (-x of)", b.String())
	result, ok := NewCuboid(
		a.Bottom,
		a.Top.
			WithX(ints.Min(a.Top.X, b.Bottom.X-1)),
	)
	if ok {
		fmt.Println("\t\tdecided on", result.String())
	} else {
		fmt.Println("\t\tfound nothing")
	}
	return result, ok
}
func rightOf(a, b *cuboid) (Cuboid, bool) {
	fmt.Println("\tfinding part of", a.String(), "which is right of (+x) of", b.String())
	result, ok := NewCuboid(
		a.Bottom.
			WithX(ints.Max(a.Bottom.X, b.Top.X+1)),
		a.Top,
	)
	if ok {
		fmt.Println("\t\tdecided on", result.String())
	} else {
		fmt.Println("\t\tfound nothing")
	}
	return result, ok
}

// behind/in front of is along the -/+ y axis, and extends infinitely in z but is limited in x to not overlap with left/right

func behind(a, b *cuboid) (Cuboid, bool) {
	fmt.Println("\tfinding part of", a.String(), "which is behind (-y of)", b.String())
	result, ok := NewCuboid(
		a.Bottom.
			WithX(ints.Max(a.Bottom.X, b.Bottom.X)),
		a.Top.
			WithX(ints.Min(a.Top.X, b.Top.X)).
			WithY(ints.Min(a.Top.Y, b.Bottom.Y-1)),
	)
	if ok {
		fmt.Println("\t\tdecided on", result.String())
	} else {
		fmt.Println("\t\tfound nothing")
	}
	return result, ok
}
func inFrontOf(a, b *cuboid) (Cuboid, bool) {
	fmt.Println("\tfinding part of", a.String(), "which is in front (+y) of", b.String())
	result, ok := NewCuboid(
		a.Bottom.
			WithX(ints.Max(a.Bottom.X, b.Bottom.X)).
			WithY(ints.Max(a.Bottom.Y, b.Top.Y+1)),
		a.Top.
			WithX(ints.Min(a.Top.X, b.Top.X)),
	)
	if ok {
		fmt.Println("\t\tdecided on", result.String())
	} else {
		fmt.Println("\t\tfound nothing")
	}
	return result, ok
}

// below/above is along the -/+ z axis, limited to only strictly below/above

func below(a, b *cuboid) (Cuboid, bool) {
	fmt.Println("\tfinding part of", a.String(), "which is below", b.String())
	result, ok := NewCuboid(
		a.Bottom.
			WithX(ints.Max(a.Bottom.X, b.Bottom.X)).
			WithY(ints.Max(a.Bottom.Y, b.Bottom.Y)),
		a.Top.
			WithX(ints.Min(a.Top.X, b.Top.X)).
			WithY(ints.Min(a.Top.Y, b.Top.Y)).
			WithZ(ints.Min(a.Top.Z, b.Bottom.Z-1)),
	)
	if ok {
		fmt.Println("\t\tdecided on", result.String())
	} else {
		fmt.Println("\t\tfound nothing")
	}
	return result, ok
}
func above(a, b *cuboid) (Cuboid, bool) {
	fmt.Println("\tfinding part of", a.String(), "which is above", b.String())
	result, ok := NewCuboid(
		a.Bottom.
			WithX(ints.Max(a.Bottom.X, b.Bottom.X)).
			WithY(ints.Max(a.Bottom.Y, b.Bottom.Y)).
			WithZ(ints.Max(a.Bottom.Z, b.Top.Z+1)),
		a.Top.
			WithX(ints.Min(a.Top.X, b.Top.X)).
			WithY(ints.Min(a.Top.Y, b.Top.Y)),
	)

	if ok {
		fmt.Println("\t\tdecided on", result.String())
	} else {
		fmt.Println("\t\tfound nothing")
	}
	return result, ok
}
func overlap(a, b *cuboid) (Cuboid, bool) {
	fmt.Println("\tfinding part of", a.String(), "which overlaps", b.String())
	result, ok := NewCuboid(
		a.Bottom.
			WithX(ints.Max(a.Bottom.X, b.Bottom.X)).
			WithY(ints.Max(a.Bottom.Y, b.Bottom.Y)).
			WithZ(ints.Max(a.Bottom.Z, b.Bottom.Z)),
		b.Top.
			WithX(ints.Min(a.Top.X, b.Top.X)).
			WithY(ints.Min(a.Top.Y, b.Top.Y)).
			WithZ(ints.Min(a.Top.Z, b.Top.Z)),
	)

	if ok {
		fmt.Println("\t\tdecided on", result.String())
	} else {
		fmt.Println("\t\tfound nothing")
	}
	return result, ok
}
