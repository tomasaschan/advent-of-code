package dec4

import (
	"testing"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

func TestDec4(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "AoC 2021")
}
