package threed

import (
	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"
)

var _ = DescribeTable("rotations",
	func(r Rotation, expected Vector) {
		Expect(r.Apply(&Vector{1, 2, 3})).To(Equal(&expected))
	},
	Entry("around x", RotX(), Vector{1, 3, -2}),
	Entry("around y", RotY(), Vector{-3, 2, 1}),
	Entry("around z", RotZ(), Vector{2, -1, 3}),

	Entry("first x, then y", RotY().After(RotX()), Vector{2, 3, 1}),
)

var _ = Describe("all rotations", func() {
	v := &Vector{X: 1, Y: 2, Z: 3}

	rotated := make([]Vector, 24)

	for i, r := range AllRotations {
		rotated[i] = *r.Apply(v)
	}

	It("lists 24 unique rotations", func() {
		Expect(rotated).To(ConsistOf(
			// all orientations pointing +x
			Vector{X: 1, Y: 2, Z: 3},
			Vector{X: 1, Y: -2, Z: -3},
			Vector{X: 1, Y: -3, Z: 2},
			Vector{X: 1, Y: 3, Z: -2},

			// all orientations pointing -y
			Vector{X: -3, Y: -1, Z: 2},
			Vector{X: 2, Y: -1, Z: 3},
			Vector{X: 3, Y: -1, Z: -2},
			Vector{X: -2, Y: -1, Z: -3},

			// all orientations pointing -x
			Vector{X: -1, Y: 3, Z: 2},
			Vector{X: -1, Y: -3, Z: -2},
			Vector{X: -1, Y: 2, Z: -3},
			Vector{X: -1, Y: -2, Z: 3},

			// all orientations pointing +y
			Vector{X: 2, Y: 1, Z: -3},
			Vector{X: 3, Y: 1, Z: 2},
			Vector{X: -2, Y: 1, Z: 3},
			Vector{X: -3, Y: 1, Z: -2},

			// all orientations pointing +z
			Vector{X: -3, Y: 2, Z: 1},
			Vector{X: 2, Y: 3, Z: 1},
			Vector{X: 3, Y: -2, Z: 1},
			Vector{X: -2, Y: -3, Z: 1},

			// all orientations pointing -z
			Vector{X: 2, Y: -3, Z: -1},
			Vector{X: 3, Y: 2, Z: -1},
			Vector{X: -2, Y: 3, Z: -1},
			Vector{X: -3, Y: -2, Z: -1},
		))
	})
})
