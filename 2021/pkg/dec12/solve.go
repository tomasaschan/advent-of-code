package dec12

import (
	"fmt"
	"regexp"
	"strings"
)

func A(input string) int {
	g := parse(input)

	paths := g.walk("start", "start", "", false)
	return len(paths)
}

func B(input string) int {
	g := parse(input)

	paths := g.walk("start", "start", "", true)
	return len(paths)
}

type graph struct {
	nodes map[string]bool
	edges map[string]map[string]bool
}

func parse(input string) graph {
	rx := regexp.MustCompile(`(\w+)-(\w+)`)

	g := graph{
		nodes: map[string]bool{},
		edges: map[string]map[string]bool{},
	}

	for _, line := range strings.Split(input, "\n") {
		if line == "" {
			continue
		}
		matches := rx.FindStringSubmatch(line)
		a, b := matches[1], matches[2]

		g.nodes[a] = true
		if _, ok := g.edges[a]; ok {
			g.edges[a][b] = true
		} else {
			g.edges[a] = map[string]bool{b: true}
		}
		g.nodes[b] = true
		if _, ok := g.edges[b]; ok {
			g.edges[b][a] = true
		} else {
			g.edges[b] = map[string]bool{a: true}
		}

	}

	return g
}

// walk returns a list of paths to the end node from here, given the path taken here (including here)
func (g graph) walk(path string, here string, smallRepeated string, allowRepeats bool) []string {
	paths := []string{}

	for next := range g.edges[here] {
		if next == "end" {
			paths = append(paths, fmt.Sprintf("%s-%s", path, "end"))
			continue
		}
		if mayGo, sr := mayGoTo(path, next, smallRepeated, allowRepeats); mayGo {
			paths = append(paths, g.walk(fmt.Sprintf("%s-%s", path, next), next, sr, allowRepeats)...)
		}
	}

	return paths
}

func mayGoTo(path string, next string, smallRepeated string, allowRepeats bool) (bool, string) {
	// we may never re-enter start, and end is handled separately
	if next == "start" || next == "end" {
		return false, ""
	}

	// it's not a small cave
	if strings.ToLower(next) != next {
		return true, smallRepeated
	}

	// it is a small cave, but we haven't been here before
	if !strings.Contains(path, fmt.Sprintf("%s-", next)) {
		return true, smallRepeated
	}

	// it is a small cave, we've been here before,
	// but we allow repeats, and havent't repeated any small cave yet
	if allowRepeats && smallRepeated == "" {
		return true, next
	}

	// it's a small cave, we'gve been here before,
	// and we've already repeated a small cave, or we don't allow repeats
	return false, smallRepeated
}
