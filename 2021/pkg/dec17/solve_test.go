package dec17_test

import (
	"io/ioutil"
	"testing"

	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"

	"github.com/tomasaschan/advent-of-code-2021/pkg/dec17"
)

var _ = Describe("Dec 17", func() {
	Context("sample", func() {
		input := `target area: x=20..30, y=-10..-5
`

		It("solves part a", func() {
			Expect(dec17.A(input)).To(Equal(45))
		})

		It("solves part b", func() {
			Expect(dec17.B(input)).To(Equal(112))
		})
	})

	Context("real input", func() {
		bytes, err := ioutil.ReadFile("../../input/dec17.txt")

		It("reads input OK", func() {
			Expect(err).NotTo(HaveOccurred())
		})

		input := string(bytes)

		It("solves part a", func() {
			Expect(dec17.A(input)).To(Equal(4656))
		})

		It("solves part b", func() {
			Expect(dec17.B(input)).To(Equal(1908))
		})
	})
})

func TestDec17(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "Dec 17")
}
