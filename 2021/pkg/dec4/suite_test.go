package dec4

import (
	"testing"

	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"
)

func TestDec4(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "Dec 4")
}
