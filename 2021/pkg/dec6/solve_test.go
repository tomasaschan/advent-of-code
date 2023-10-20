package dec6_test

import (
	"io/ioutil"
	"testing"

	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"
	"github.com/tomasaschan/advent-of-code-2021/pkg/dec6"
)

var _ = Describe("Dec 6", func() {
	Context("sample", func() {
		input := "3,4,3,1,2"

		It("solves part a", func() {
			Expect(dec6.A(input)).To(Equal(5934))
		})

		It("solves part b", func() {
			Expect(dec6.B(input)).To(Equal(26984457539))
		})
	})

	Context("real input", func() {
		input, err := ioutil.ReadFile("../../input/dec6.txt")
		Expect(err).NotTo(HaveOccurred())

		It("solves part a", func() {
			Expect(dec6.A(string(input))).To(Equal(343441))
		})

		It("solves part b", func() {
			Expect(dec6.B(string(input))).To(Equal(1569108373832))
		})
	})
})

func TestDec6(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "Dec 6")
}
