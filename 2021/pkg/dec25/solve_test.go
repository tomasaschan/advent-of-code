package dec25_test

import (
	"io/ioutil"
	"testing"

	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"

	"github.com/tomasaschan/advent-of-code-2021/pkg/dec25"
)

var _ = Describe("Dec 25", func() {
	Context("sample", func() {
		input := `
v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>
`

		It("solves part a", func() {
			Expect(dec25.A(input)).To(Equal(58))
		})
	})

	Context("real input", func() {
		bytes, err := ioutil.ReadFile("../../input/dec25.txt")

		It("reads input OK", func() {
			Expect(err).NotTo(HaveOccurred())
		})

		input := string(bytes)

		It("solves part a", func() {
			Expect(dec25.A(input)).To(Equal(528))
		})
	})
})

func TestDec22(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "Dec 25")
}
