#!/bin/bash

day=${1?"usage: $0 day"}

mkdir -p "pkg/dec$day"
mkdir -p input
touch "input/dec$day.txt"

if ! [[ -f "pkg/dec$day/solve.go" ]]; then
    printf "package dec%d

func A(input string) int {
\treturn 0
}

func B(input string) int {
\treturn 0
}
" "$day" > "pkg/dec$day/solve.go"
else
    echo "pkg/dec$day/solve.go already exists; not overwriting"
fi

if ! [[ -f "pkg/dec${day}/solve_test.go" ]]; then
    printf "package dec%s_test

import (
	\"io/ioutil\"
	\"testing\"

	. \"github.com/onsi/ginkgo/v2\"
	. \"github.com/onsi/gomega\"

	\"github.com/tomasaschan/advent-of-code-2021/pkg/dec%s\"
)

var _ = Describe(\"Dec %s\", func() {
	Context(\"sample\", func() {
		input := \`\`

		It(\"solves part a\", func() {
			Expect(dec%s.A(input)).To(Equal(0))
		})

		It(\"solves part b\", func() {
			Expect(dec%s.B(input)).To(Equal(0))
		})
	})

	Context(\"real input\", func() {
		bytes, err := ioutil.ReadFile(\"../../input/dec%s.txt\")
		
		It(\"reads input OK\", func() {
			Expect(err).NotTo(HaveOccurred())
		})

		input := string(bytes)

		It(\"solves part a\", func() {
			Expect(dec%s.A(input)).To(Equal(0))
		})

		It(\"solves part b\", func() {
			Expect(dec%s.B(input)).To(Equal(0))
		})
	})
})

func TestDec22(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, \"Dec %s\")
}
" "$day" "$day" "$day" "$day" "$day" "$day" "$day" "$day" "$day" > "pkg/dec${day}/solve_test.go"
else
    echo "pkg/dec${day}/solve_test.go already exists; not overwriting"
fi
