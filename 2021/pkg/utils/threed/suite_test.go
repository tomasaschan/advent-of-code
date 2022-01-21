package threed

import (
	"testing"

	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"
)

func TestLinalg3D(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "3D Linear Algebra")
}
