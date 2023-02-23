package cuboid_test

import (
	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"
	"github.com/tomasaschan/advent-of-code-2021/pkg/dec22/cuboid"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/threed"
)

var _ = Describe("cuboid", func() {
	DescribeTable(
		"splitting",
		func(c cuboid.Cuboid, splitPoint threed.Point, parts []cuboid.Cuboid) {
			Expect(c.Subdivide(splitPoint)).To(ConsistOf(parts))
		},
		Entry(
			"split point in the inner domain",
			cuboid.Cuboid{threed.Point{-3, -3, -3}, threed.Point{3, 3, 3}},
			threed.Point{0, 0, 0},
			// The octant is a number from 1 to 8:
			//
			//   z > 0         z < 0
			//
			//     y             y
			//   2 | 1         6 | 5
			//  ---+--- x     ---+--- x
			//   3 | 4         7 | 8
			[]cuboid.Cuboid{
				{threed.Point{0, 0, 0}, threed.Point{0, 0, 0}}, // split point itself

				{threed.Point{-3, 0, 0}, threed.Point{-1, 0, 0}}, // along negative x
				{threed.Point{1, 0, 0}, threed.Point{3, 0, 0}},   // along positive x
				{threed.Point{0, -3, 0}, threed.Point{0, -1, 0}}, // along negative y
				{threed.Point{0, 1, 0}, threed.Point{0, 3, 0}},   // along positive y
				{threed.Point{0, 0, -3}, threed.Point{0, 0, -1}}, // along negative z
				{threed.Point{0, 0, 1}, threed.Point{0, 0, 3}},   // along positive z

				{threed.Point{0, -3, 1}, threed.Point{0, -1, 3}},   //plane in -y/+z
				{threed.Point{0, 1, 1}, threed.Point{0, 3, 3}},     //plane in +y/+z
				{threed.Point{0, -3, -3}, threed.Point{0, -1, -1}}, //plane in -y/-z
				{threed.Point{0, 1, -3}, threed.Point{0, 3, -1}},   //plane in +y/-z
				{threed.Point{-3, 0, 1}, threed.Point{-1, 0, 3}},   // plane in -x/+z
				{threed.Point{1, 0, 1}, threed.Point{3, 0, 3}},     // plane in +x/+z
				{threed.Point{-3, 0, -3}, threed.Point{-1, 0, -1}}, // plane in -x/-z
				{threed.Point{1, 0, -3}, threed.Point{3, 0, -1}},   // plane in +x/-z
				{threed.Point{-3, 1, 0}, threed.Point{-1, 3, 0}},   // plane in -x/+y
				{threed.Point{1, 1, 0}, threed.Point{3, 3, 0}},     // plane in +x/+y
				{threed.Point{-3, -3, 0}, threed.Point{-1, -1, 0}}, // plane in -x/-y
				{threed.Point{1, -3, 0}, threed.Point{3, -1, 0}},   // plane in +x/-y

				{threed.Point{1, 1, 1}, threed.Point{3, 3, 3}},       // octant 1
				{threed.Point{-3, 1, 1}, threed.Point{-1, 3, 3}},     // octant 2
				{threed.Point{-3, -3, 1}, threed.Point{-1, -1, 3}},   // octant 3
				{threed.Point{1, -3, 1}, threed.Point{3, -1, 3}},     // octant 4
				{threed.Point{1, 1, -3}, threed.Point{3, 3, -1}},     // octant 5
				{threed.Point{-3, 1, -3}, threed.Point{-1, 3, -1}},   // octant 6
				{threed.Point{-3, -3, -3}, threed.Point{-1, -1, -1}}, // octant 7
				{threed.Point{1, -3, -3}, threed.Point{3, -1, -1}},   // octant 8
			},
		),
		Entry(
			"split point on a side",
			cuboid.Cuboid{threed.Point{-3, 0, -3}, threed.Point{3, 3, 3}},
			threed.Point{0, 0, 0},
			// The octant is a number from 1 to 8:
			//
			//   z > 0         z < 0
			//
			//     y             y
			//   2 | 1         6 | 5
			//  ---+--- x     ---+--- x
			//   3 | 4         7 | 8
			[]cuboid.Cuboid{
				{threed.Point{0, 0, 0}, threed.Point{0, 0, 0}}, // split point itself

				{threed.Point{-3, 0, 0}, threed.Point{-1, 0, 0}}, // along negative x
				{threed.Point{1, 0, 0}, threed.Point{3, 0, 0}},   // along positive x
				{threed.Point{0, 1, 0}, threed.Point{0, 3, 0}},   // along positive y
				{threed.Point{0, 0, -3}, threed.Point{0, 0, -1}}, // along negative z
				{threed.Point{0, 0, 1}, threed.Point{0, 0, 3}},   // along positive z

				{threed.Point{0, 1, 1}, threed.Point{0, 3, 3}},     //plane in +y/+z
				{threed.Point{0, 1, -3}, threed.Point{0, 3, -1}},   //plane in +y/-z
				{threed.Point{-3, 0, 1}, threed.Point{-1, 0, 3}},   // plane in -x/+z
				{threed.Point{1, 0, 1}, threed.Point{3, 0, 3}},     // plane in +x/+z
				{threed.Point{-3, 0, -3}, threed.Point{-1, 0, -1}}, // plane in -x/-z
				{threed.Point{1, 0, -3}, threed.Point{3, 0, -1}},   // plane in +x/-z
				{threed.Point{-3, 1, 0}, threed.Point{-1, 3, 0}},   // plane in -x/+y
				{threed.Point{1, 1, 0}, threed.Point{3, 3, 0}},     // plane in +x/+y

				{threed.Point{1, 1, 1}, threed.Point{3, 3, 3}},     // octant 1
				{threed.Point{-3, 1, 1}, threed.Point{-1, 3, 3}},   // octant 2
				{threed.Point{1, 1, -3}, threed.Point{3, 3, -1}},   // octant 5
				{threed.Point{-3, 1, -3}, threed.Point{-1, 3, -1}}, // octant 6
			},
		),
		Entry(
			"split point on an edge",
			cuboid.Cuboid{threed.Point{-3, 0, 0}, threed.Point{3, 3, 3}},
			threed.Point{0, 0, 0},
			// The octant is a number from 1 to 8:
			//
			//   z > 0         z < 0
			//
			//     y             y
			//   2 | 1         6 | 5
			//  ---+--- x     ---+--- x
			//   3 | 4         7 | 8
			[]cuboid.Cuboid{
				{threed.Point{0, 0, 0}, threed.Point{0, 0, 0}}, // split point itself

				{threed.Point{-3, 0, 0}, threed.Point{-1, 0, 0}}, // along negative x
				{threed.Point{1, 0, 0}, threed.Point{3, 0, 0}},   // along positive x
				{threed.Point{0, 1, 0}, threed.Point{0, 3, 0}},   // along positive y
				{threed.Point{0, 0, 1}, threed.Point{0, 0, 3}},   // along positive z

				{threed.Point{0, 1, 1}, threed.Point{0, 3, 3}},   //plane in +y/+z
				{threed.Point{-3, 0, 1}, threed.Point{-1, 0, 3}}, // plane in -x/+z
				{threed.Point{1, 0, 1}, threed.Point{3, 0, 3}},   // plane in +x/+z
				{threed.Point{-3, 1, 0}, threed.Point{-1, 3, 0}}, // plane in -x/+y
				{threed.Point{1, 1, 0}, threed.Point{3, 3, 0}},   // plane in +x/+y

				{threed.Point{1, 1, 1}, threed.Point{3, 3, 3}},   // octant 1
				{threed.Point{-3, 1, 1}, threed.Point{-1, 3, 3}}, // octant 2
			},
		),
		Entry(
			"split point on a corner",
			cuboid.Cuboid{threed.Point{0, 0, 0}, threed.Point{3, 3, 3}},
			threed.Point{0, 0, 0},
			// The octant is a number from 1 to 8:
			//
			//   z > 0         z < 0
			//
			//     y             y
			//   2 | 1         6 | 5
			//  ---+--- x     ---+--- x
			//   3 | 4         7 | 8
			[]cuboid.Cuboid{
				{threed.Point{0, 0, 0}, threed.Point{0, 0, 0}}, // split point itself

				{threed.Point{1, 0, 0}, threed.Point{3, 0, 0}}, // along positive x
				{threed.Point{0, 1, 0}, threed.Point{0, 3, 0}}, // along positive y
				{threed.Point{0, 0, 1}, threed.Point{0, 0, 3}}, // along positive z

				{threed.Point{0, 1, 1}, threed.Point{0, 3, 3}}, //plane in +y/+z
				{threed.Point{1, 0, 1}, threed.Point{3, 0, 3}}, // plane in +x/+z
				{threed.Point{1, 1, 0}, threed.Point{3, 3, 0}}, // plane in +x/+y

				{threed.Point{1, 1, 1}, threed.Point{3, 3, 3}}, // octant 1
			},
		),
	)
})
