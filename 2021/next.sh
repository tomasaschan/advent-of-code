#!/bin/bash

day=${1?"usage: $0 day"}

mkdir -p "pkg/dec$day"
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

if ! [[ -f "dec${day}_test.go" ]]; then
    printf "package aoc2021

import (
	\"io/ioutil\"

	. \"github.com/onsi/ginkgo\"
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
		bytes, err := ioutil.ReadFile(\"input/dec%s.txt\")
		
		It("reads input OK", func() {
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
" "$day" "$day" "$day" "$day" "$day" "$day" "$day" > "dec${day}_test.go"
else
    echo "dec${day}_test.go already exists; not overwriting"
fi
