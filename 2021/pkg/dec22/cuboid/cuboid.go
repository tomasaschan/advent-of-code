package cuboid

import (
	"fmt"

	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/ints"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/threed"
)

type Cuboid struct {
	Bottom threed.Point
	Top    threed.Point
}

type Corner struct {
	Location threed.Point
	Octant   int
}

func (c Cuboid) Contains(p threed.Point) bool {
	return c.Bottom.X <= p.X && p.X <= c.Top.X &&
		c.Bottom.Y <= p.Y && p.Y <= c.Top.Y &&
		c.Bottom.Z <= p.Z && p.Z <= c.Top.Z
}

func (c Cuboid) StrictlyContains(p threed.Point) bool {
	return c.Bottom.X < p.X && p.X < c.Top.X &&
		c.Bottom.Y < p.Y && p.Y < c.Top.Y &&
		c.Bottom.Z < p.Z && p.Z < c.Top.Z
}

// Corners returns the locations of the 8 corners of the cuboid, and an
// int indicating which direction the corner is in.
//
// The octant is a number from 1 to 8:
//
//   z > 0         z < 0
//
//     y             y
//   2 | 1         6 | 5
//  ---+--- x     ---+--- x
//   3 | 4         7 | 8
// func (c Cuboid) Corners() []Corner {
// 	return []Corner{
// 		{Location: c.Top, Octant: 1},
// 		{Location: c.Top.WithX(c.Bottom.X), Octant: 2},
// 		{Location: c.Bottom.WithZ(c.Top.Z), Octant: 3},
// 		{Location: c.Top.WithY(c.Bottom.Y), Octant: 4},
// 		{Location: c.Top.WithZ(c.Bottom.Z), Octant: 5},
// 		{Location: c.Bottom.WithY(c.Top.Y), Octant: 6},
// 		{Location: c.Bottom, Octant: 7},
// 		{Location: c.Bottom.WithX(c.Top.X), Octant: 8},
// 	}
// }

func (c Cuboid) Corners() []threed.Point {
	return []threed.Point{
		c.Top,
		c.Top.WithX(c.Bottom.X),
		c.Bottom.WithZ(c.Top.Z),
		c.Top.WithY(c.Bottom.Y),
		c.Top.WithZ(c.Bottom.Z),
		c.Bottom.WithY(c.Top.Y),
		c.Bottom,
		c.Bottom.WithX(c.Top.X),
	}
}

func (c Cuboid) IsCorner(p threed.Point) bool {
	for _, corner := range c.Corners() {
		if p == corner {
			return true
		}
	}
	return false
}

func (c Cuboid) OverlapWith(other Cuboid) (Cuboid, bool) {
	if c.Bottom.X > other.Top.X ||
		c.Bottom.Y > other.Top.Y ||
		c.Bottom.Z > other.Top.Z ||
		c.Top.X < other.Bottom.X ||
		c.Top.Y < other.Bottom.Y ||
		c.Top.Z < other.Bottom.Z {
		return Cuboid{}, false
	}

	return Cuboid{
		threed.Point{
			ints.Max(c.Bottom.X, other.Bottom.X),
			ints.Max(c.Bottom.Y, other.Bottom.Y),
			ints.Max(c.Bottom.Z, other.Bottom.Z),
		},
		threed.Point{
			ints.Min(c.Top.X, other.Top.X),
			ints.Min(c.Top.Y, other.Top.Y),
			ints.Min(c.Top.Z, other.Top.Z),
		},
	}, true
}

func (c Cuboid) Size() int {
	return (1 + c.Top.X - c.Bottom.X) * (1 + c.Top.Y - c.Bottom.Y) * (1 + c.Top.Z - c.Bottom.Z)
}

func (c Cuboid) Subdivide(splitPoint threed.Point) []Cuboid {
	if c.Size() == 1 {
		panic("subdividing cuboid of size 1")
	}
	if !c.Contains(splitPoint) {
		panic(fmt.Sprintf("subdividing cuboid %v at %v, which is not inside", c, splitPoint))
	}

	parts := []Cuboid{{Bottom: splitPoint, Top: splitPoint}}

	parts = append(parts, c.subdivdeAxes(splitPoint)...)
	parts = append(parts, c.subdividePlanes(splitPoint)...)
	parts = append(parts, c.subdivideOctants(splitPoint)...)

	return parts
}

func (c Cuboid) subdivdeAxes(splitPoint threed.Point) []Cuboid {
	parts := []Cuboid{}
	if c.Bottom.X < splitPoint.X {
		// along negative x
		parts = append(parts,
			Cuboid{
				threed.Point{c.Bottom.X, splitPoint.Y, splitPoint.Z},
				threed.Point{splitPoint.X - 1, splitPoint.Y, splitPoint.Z},
			},
		)
	}
	if c.Top.X > splitPoint.X {
		// along positive x
		parts = append(parts,
			Cuboid{
				threed.Point{splitPoint.X + 1, splitPoint.Y, splitPoint.Z},
				threed.Point{c.Top.X, splitPoint.Y, splitPoint.Z},
			},
		)
	}
	if c.Bottom.Y < splitPoint.Y {
		// along negative y
		parts = append(parts,
			Cuboid{
				threed.Point{splitPoint.X, c.Bottom.Y, splitPoint.Z},
				threed.Point{splitPoint.X, splitPoint.Y - 1, splitPoint.Z},
			},
		)
	}
	if c.Top.Y > splitPoint.Y {
		// along positive y
		parts = append(parts,
			Cuboid{
				threed.Point{splitPoint.X, splitPoint.Y + 1, splitPoint.Z},
				threed.Point{splitPoint.X, c.Top.Y, splitPoint.Z},
			},
		)
	}
	if c.Bottom.Z < splitPoint.Z {
		// along negative z
		parts = append(parts,
			Cuboid{
				threed.Point{splitPoint.X, splitPoint.Y, c.Bottom.Z},
				threed.Point{splitPoint.X, splitPoint.Y, splitPoint.Z - 1},
			},
		)
	}
	if c.Top.Z > splitPoint.Z {
		// along positive z
		parts = append(parts,
			Cuboid{
				threed.Point{splitPoint.X, splitPoint.Y, splitPoint.Z + 1},
				threed.Point{splitPoint.X, splitPoint.Y, c.Top.Z},
			},
		)
	}

	return parts
}

func (c Cuboid) subdividePlanes(splitPoint threed.Point) []Cuboid {
	parts := []Cuboid{}

	// xy-planes
	{
		if c.Top.X > splitPoint.X && c.Top.Y > splitPoint.Y {
			parts = append(parts,
				Cuboid{
					threed.Point{splitPoint.X + 1, splitPoint.Y + 1, splitPoint.Z},
					threed.Point{c.Top.X, c.Top.Y, splitPoint.Z},
				},
			)
		}

		if c.Bottom.X < splitPoint.X && c.Top.Y > splitPoint.Y {
			parts = append(parts,
				Cuboid{
					threed.Point{c.Bottom.X, splitPoint.Y + 1, splitPoint.Z},
					threed.Point{splitPoint.X - 1, c.Top.Y, splitPoint.Z},
				},
			)
		}

		if c.Bottom.X < splitPoint.X && c.Bottom.Y < splitPoint.Y {
			parts = append(parts,
				Cuboid{
					threed.Point{c.Bottom.X, c.Bottom.Y, splitPoint.Z},
					threed.Point{splitPoint.X - 1, splitPoint.Y - 1, splitPoint.Z},
				},
			)
		}

		if c.Top.X > splitPoint.X && c.Bottom.Y < splitPoint.Y {
			parts = append(parts,
				Cuboid{
					threed.Point{splitPoint.X + 1, c.Bottom.Y, splitPoint.Z},
					threed.Point{c.Top.X, splitPoint.Y - 1, splitPoint.Z},
				},
			)
		}
	}

	// xz-planes
	{
		if c.Top.X > splitPoint.X && c.Top.Z > splitPoint.Z {
			parts = append(parts,
				Cuboid{
					threed.Point{splitPoint.X + 1, splitPoint.Y, splitPoint.Z + 1},
					threed.Point{c.Top.X, splitPoint.Y, c.Top.Z},
				},
			)
		}

		if c.Bottom.X < splitPoint.X && c.Top.Z > splitPoint.Z {
			parts = append(parts,
				Cuboid{
					threed.Point{c.Bottom.X, splitPoint.Y, splitPoint.Z + 1},
					threed.Point{splitPoint.X - 1, splitPoint.Y, c.Top.Z},
				},
			)
		}

		if c.Bottom.X < splitPoint.X && c.Bottom.Z < splitPoint.Z {
			parts = append(parts,
				Cuboid{
					threed.Point{c.Bottom.X, splitPoint.Y, c.Bottom.Z},
					threed.Point{splitPoint.X - 1, splitPoint.Y, splitPoint.Z - 1},
				},
			)
		}

		if c.Top.X > splitPoint.X && c.Bottom.Z < splitPoint.Z {
			parts = append(parts,
				Cuboid{
					threed.Point{splitPoint.X + 1, splitPoint.Y, c.Bottom.Z},
					threed.Point{c.Top.X, splitPoint.Y, splitPoint.Z - 1},
				},
			)
		}
	}

	// yz-planes
	{
		if c.Top.Y > splitPoint.Y && c.Top.Z > splitPoint.Z {
			parts = append(parts,
				Cuboid{
					threed.Point{splitPoint.X, splitPoint.Y + 1, splitPoint.Z + 1},
					threed.Point{splitPoint.X, c.Top.Y, c.Top.Z},
				},
			)
		}

		if c.Bottom.Y < splitPoint.Y && c.Top.Z > splitPoint.Z {
			parts = append(parts,
				Cuboid{
					threed.Point{splitPoint.X, c.Bottom.Y, splitPoint.Z + 1},
					threed.Point{splitPoint.X, splitPoint.Y - 1, c.Top.Z},
				},
			)
		}

		if c.Bottom.Y < splitPoint.Y && c.Bottom.Z < splitPoint.Z {
			parts = append(parts,
				Cuboid{
					threed.Point{splitPoint.X, c.Bottom.Y, c.Bottom.Z},
					threed.Point{splitPoint.X, splitPoint.Y - 1, splitPoint.Z - 1},
				},
			)
		}

		if c.Top.Y > splitPoint.Y && c.Bottom.Z < splitPoint.Z {
			parts = append(parts,
				Cuboid{
					threed.Point{splitPoint.X, splitPoint.Y + 1, c.Bottom.Z},
					threed.Point{splitPoint.X, c.Top.Y, splitPoint.Z - 1},
				},
			)
		}
	}

	return parts
}

func (c Cuboid) subdivideOctants(splitPoint threed.Point) []Cuboid {
	parts := []Cuboid{}

	if c.Top.X > splitPoint.X && c.Top.Y > splitPoint.Y && c.Top.Z > splitPoint.Z {
		parts = append(parts, Cuboid{
			threed.Point{splitPoint.X + 1, splitPoint.Y + 1, splitPoint.Z + 1},
			c.Top,
		})
	}
	if c.Bottom.X < splitPoint.X && c.Top.Y > splitPoint.Y && c.Top.Z > splitPoint.Z {
		parts = append(parts, Cuboid{
			threed.Point{c.Bottom.X, splitPoint.Y + 1, splitPoint.Z + 1},
			threed.Point{splitPoint.X - 1, c.Top.Y, c.Top.Z},
		})
	}
	if c.Bottom.X < splitPoint.X && c.Bottom.Y < splitPoint.Y && c.Top.Z > splitPoint.Z {
		parts = append(parts, Cuboid{
			threed.Point{c.Bottom.X, c.Bottom.Y, splitPoint.Z + 1},
			threed.Point{splitPoint.X - 1, splitPoint.Y - 1, c.Top.Z},
		})
	}
	if c.Top.X > splitPoint.X && c.Bottom.Y < splitPoint.Y && c.Top.Z > splitPoint.Z {
		parts = append(parts, Cuboid{
			threed.Point{splitPoint.X + 1, c.Bottom.Y, splitPoint.Z + 1},
			threed.Point{c.Top.X, splitPoint.Y - 1, c.Top.Z},
		})
	}

	if c.Top.X > splitPoint.X && c.Top.Y > splitPoint.Y && c.Bottom.Z < splitPoint.Z {
		parts = append(parts, Cuboid{
			threed.Point{splitPoint.X + 1, splitPoint.Y + 1, c.Bottom.Z},
			threed.Point{c.Top.X, c.Top.Y, splitPoint.Z - 1},
		})
	}
	if c.Bottom.X < splitPoint.X && c.Top.Y > splitPoint.Y && c.Bottom.Z < splitPoint.Z {
		parts = append(parts, Cuboid{
			threed.Point{c.Bottom.X, splitPoint.Y + 1, c.Bottom.Z},
			threed.Point{splitPoint.X - 1, c.Top.Y, splitPoint.Z - 1},
		})
	}
	if c.Bottom.X < splitPoint.X && c.Bottom.Y < splitPoint.Y && c.Bottom.Z < splitPoint.Z {
		parts = append(parts, Cuboid{
			threed.Point{c.Bottom.X, c.Bottom.Y, c.Bottom.Z},
			threed.Point{splitPoint.X - 1, splitPoint.Y - 1, splitPoint.Z - 1},
		})
	}
	if c.Top.X > splitPoint.X && c.Bottom.Y < splitPoint.Y && c.Bottom.Z < splitPoint.Z {
		parts = append(parts, Cuboid{
			threed.Point{splitPoint.X + 1, c.Bottom.Y, c.Bottom.Z},
			threed.Point{c.Top.X, splitPoint.Y - 1, splitPoint.Z - 1},
		})
	}

	return parts
}

// func splitDimension(bottom, top, splitPoint, octant int) (bottoms, tops []int) {
// 	bottoms = append(bottoms, bottom)

// 	if bottom == top {
// 		tops = append(tops, top)
// 		return
// 	}

// 	if (top-bottom)%2 == 1 {
// 		bottoms = append(bottoms, bottom+(top-bottom)/2+1)
// 		tops = append(tops, bottom+(top-bottom)/2, top)
// 		return
// 	}

// 	if (top-bottom)%2 == 0 {
// 		bottoms = append(bottoms, bottom+(top-bottom)/2)
// 		tops = append(tops, bottom+(top-bottom)/2-1, top)
// 		return
// 	}

// 	panic("this should never happen")
// }

func (c Cuboid) String() string {
	return fmt.Sprintf("Cuboid({%d,%d,%d}, {%d,%d,%d})", c.Bottom.X, c.Bottom.Y, c.Bottom.Z, c.Top.X, c.Top.Y, c.Top.Z)
}

// left/right of is along the -/+ x-axis, and extends infinitely in y and z

// func leftOf(a, b cuboid) (Cuboid, bool) {
// 	result, ok := NewCuboid(
// 		a.bottom,
// 		a.top.
// 			WithX(ints.Min(a.top.X, b.bottom.X-1)),
// 	)

// 	return result, ok
// }
// func rightOf(a, b cuboid) (Cuboid, bool) {
// 	result, ok := NewCuboid(
// 		a.bottom.
// 			WithX(ints.Max(a.bottom.X, b.top.X+1)),
// 		a.top,
// 	)

// 	return result, ok
// }

// behind/in front of is along the -/+ y axis, and extends infinitely in z but is limited in x to not overlap with left/right

// func behind(a, b Cuboid) Cuboid {
// 	result, ok := NewCuboid(
// 		a.bottom.
// 			WithX(ints.Max(a.bottom.X, b.bottom.X)),
// 		a.top.
// 			WithX(ints.Min(a.top.X, b.top.X)).
// 			WithY(ints.Min(a.top.Y, b.bottom.Y-1)),
// 	)

// 	return result, ok
// }
// func inFrontOf(a, b Cuboid) Cuboid {
// 	return Cuboid{bottom:
// 		a.bottom.
// 			WithX(ints.Max(a.bottom.X, b.bottom.X)).
// 			WithY(ints.Max(a.bottom.Y, b.top.Y+1)),
// 		top: a.top.
// 			WithX(ints.Min(a.top.X, b.top.X)),
// 	}
// }

// below/above is along the -/+ z axis, limited to only strictly below/above

// func below(a, b cuboid) (Cuboid, bool) {
// 	result, ok := NewCuboid(
// 		a.bottom.
// 			WithX(ints.Max(a.bottom.X, b.bottom.X)).
// 			WithY(ints.Max(a.bottom.Y, b.bottom.Y)),
// 		a.top.
// 			WithX(ints.Min(a.top.X, b.top.X)).
// 			WithY(ints.Min(a.top.Y, b.top.Y)).
// 			WithZ(ints.Min(a.top.Z, b.bottom.Z-1)),
// 	)

// 	return result, ok
// }
// func above(a, b cuboid) (Cuboid, bool) {
// 	result, ok := NewCuboid(
// 		a.bottom.
// 			WithX(ints.Max(a.bottom.X, b.bottom.X)).
// 			WithY(ints.Max(a.bottom.Y, b.bottom.Y)).
// 			WithZ(ints.Max(a.bottom.Z, b.top.Z+1)),
// 		a.top.
// 			WithX(ints.Min(a.top.X, b.top.X)).
// 			WithY(ints.Min(a.top.Y, b.top.Y)),
// 	)

// 	return result, ok
// }

// func overlap(a, b cuboid) (Cuboid, bool) {
// 	result, ok := NewCuboid(
// 		a.bottom.
// 			WithX(ints.Max(a.bottom.X, b.bottom.X)).
// 			WithY(ints.Max(a.bottom.Y, b.bottom.Y)).
// 			WithZ(ints.Max(a.bottom.Z, b.bottom.Z)),
// 		b.top.
// 			WithX(ints.Min(a.top.X, b.top.X)).
// 			WithY(ints.Min(a.top.Y, b.top.Y)).
// 			WithZ(ints.Min(a.top.Z, b.top.Z)),
// 	)

// 	return result, ok
// }
