package cuboid_test

import (
	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"
	"github.com/tomasaschan/advent-of-code-2021/pkg/dec22/cuboid"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/threed"
)

var _ = Describe("cuboid", func() {
	DescribeTable("overlap detection",
		func(a, b cuboid.Cuboid, onlyA, overlap, onlyB cuboid.CuboidSet) {
			oa, ov, ob := a.FindOverlap(b)
			Expect(oa.String()).To(Equal(onlyA.String()), "only a")
			Expect(ov.String()).To(Equal(overlap.String()), "overlap")
			Expect(ob.String()).To(Equal(onlyB.String()), "only b")
		},
		Entry("no overlap",
			makeCuboid(-5, -5, -5, -1, -1, -1), makeCuboid(1, 1, 1, 5, 5, 5),
			cuboid.NewSet().With(makeCuboid(-5, -5, -5, -1, -1, -1)),
			cuboid.NewSet(),
			cuboid.NewSet().With(makeCuboid(1, 1, 1, 5, 5, 5)),
		),
		Entry("100%% overlap",
			makeCuboid(0, 0, 0, 3, 3, 3),
			makeCuboid(0, 0, 0, 3, 3, 3),
			cuboid.NewSet(),
			cuboid.NewSet().With(makeCuboid(0, 0, 0, 3, 3, 3)),
			cuboid.NewSet(),
		),
		Entry("a behind b",
			makeCuboid(-1, 0, 0, 3, 3, 3),
			makeCuboid(0, 0, 0, 3, 3, 3),
			cuboid.NewSet().With(makeCuboid(-1, 0, 0, -1, 3, 3)),
			cuboid.NewSet().With(makeCuboid(0, 0, 0, 3, 3, 3)),
			cuboid.NewSet(),
		),
		Entry("a in front of b",
			makeCuboid(0, 0, 0, 4, 3, 3),
			makeCuboid(0, 0, 0, 3, 3, 3),
			cuboid.NewSet().With(makeCuboid(4, 0, 0, 4, 3, 3)),
			cuboid.NewSet().With(makeCuboid(0, 0, 0, 3, 3, 3)),
			cuboid.NewSet(),
		),
		Entry("a left of b",
			makeCuboid(0, -1, 0, 3, 3, 3),
			makeCuboid(0, 0, 0, 3, 3, 3),
			cuboid.NewSet().With(makeCuboid(0, -1, 0, 3, -1, 3)),
			cuboid.NewSet().With(makeCuboid(0, 0, 0, 3, 3, 3)),
			cuboid.NewSet(),
		),
		Entry("a right of b",
			makeCuboid(0, 0, 0, 3, 4, 3),
			makeCuboid(0, 0, 0, 3, 3, 3),
			cuboid.NewSet().With(makeCuboid(0, 4, 0, 3, 4, 3)),
			cuboid.NewSet().With(makeCuboid(0, 0, 0, 3, 3, 3)),
			cuboid.NewSet(),
		),
		Entry("a below b",
			makeCuboid(0, 0, -1, 3, 3, 3),
			makeCuboid(0, 0, 0, 3, 3, 3),
			cuboid.NewSet().With(makeCuboid(0, 0, -1, 3, 3, -1)),
			cuboid.NewSet().With(makeCuboid(0, 0, 0, 3, 3, 3)),
			cuboid.NewSet(),
		),
		Entry("a above b",
			makeCuboid(0, 0, 0, 3, 3, 4),
			makeCuboid(0, 0, 0, 3, 3, 3),
			cuboid.NewSet().With(makeCuboid(0, 0, 4, 3, 3, 4)),
			cuboid.NewSet().With(makeCuboid(0, 0, 0, 3, 3, 3)),
			cuboid.NewSet(),
		),
	)
})

func makeCuboid(x0, y0, z0, x1, y1, z1 int) cuboid.Cuboid {
	defer GinkgoRecover()
	c, ok := cuboid.NewCuboid(&threed.Point{X: x0, Y: y0, Z: z0}, &threed.Point{X: x1, Y: y1, Z: z1})
	Expect(ok).To(BeTrue())
	return c
}
