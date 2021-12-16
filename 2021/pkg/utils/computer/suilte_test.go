package computer

import (
	"testing"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

func TestAoC2021(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "Computer")
}
