package threed

import (
	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"
)

var _ = DescribeTable("matrix x vector",
	func(m *matrix, v *Vector, expected *Vector) {
		Expect(m.times(v)).To(Equal(expected))
	},
	Entry("zero vector", Matrix(1, 2, 3, 4, 5, 6, 7, 8, 9), &Vector{X: 0, Y: 0, Z: 0}, &Vector{X: 0, Y: 0, Z: 0}),
	Entry("identity matrix", Matrix(1, 0, 0, 0, 1, 0, 0, 0, 1), &Vector{X: 1, Y: 2, Z: 3}, &Vector{X: 1, Y: 2, Z: 3}),
	Entry("more complex sample",
		Matrix(
			1, 1, 2,
			2, 1, 3,
			1, 4, 2,
		),
		&Vector{3, 1, 2},
		&Vector{8, 13, 11},
	),
)
