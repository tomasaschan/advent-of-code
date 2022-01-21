package aoc2021

import (
	"io/ioutil"

	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"

	"github.com/tomasaschan/advent-of-code-2021/pkg/dec12"
)

var _ = Describe("Dec 12", func() {
	bytes, err := ioutil.ReadFile("input/dec12.txt")

	It("reads input OK", func() {
		Expect(err).NotTo(HaveOccurred())
	})

	input := string(bytes)

	DescribeTable("solves all",
		func(input string, a int, b int) {
			By("solve part a")
			Expect(dec12.A(input)).To(Equal(a))

			By("solve part b")
			Expect(dec12.B(input)).To(Equal(b))

		},
		Entry("small sample", "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end\n", 10, 36),
		Entry("sligthly larger sample", "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc\n", 19, 103),
		Entry("even larger sample", "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW\n", 226, 3509),
		Entry("real data", input, 3410, 98796),
	)
})
