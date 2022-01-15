package dec19

import (
	"fmt"
	"strings"

	"github.com/tomasaschan/advent-of-code-2021/pkg/utils"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/ints"
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/threed"
)

func A(input string) int {
	data := parse(input)

	beacons := data[0]

	rest := data[1:]

	for i := 0; len(rest) > 0; i++ {
		next, l := rest[0], len(*beacons)
		beacons.extend(next)
		if len(*beacons) != l {
			// we learned something; move on to the next one
			rest = rest[1:]
		} else {
			// we learned nothing; requeue this one and move on
			rest = append(rest[1:], next)
		}
	}

	return len(*beacons)
}

func B(input string) int {
	data := parse(input)

	beacons := data[0]
	scanners := make([]threed.Vector, 0)

	rest := data[1:]

	for i := 0; len(rest) > 0; i++ {
		next := rest[0]
		if t, ok := beacons.extend(next); ok {
			// we learned something; move on to the next one
			rest = rest[1:]
			scanners = append(scanners, t.translation)
		} else {
			// we learned nothing; requeue this one and move on
			rest = append(rest[1:], next)
		}
	}

	maxDistance := 0
	for i := range scanners {
		for j := range scanners[i+1:] {
			a, b := scanners[i], scanners[i+j+1]
			d := ints.Abs(a.X-b.X) + ints.Abs(a.Y-b.Y) + ints.Abs(a.Z-b.Z)
			if d > maxDistance {
				maxDistance = d
			}
		}
	}

	return maxDistance
}

func parse(input string) []*beacons {
	scanners := []*beacons{}

	for i, dump := range strings.Split(strings.TrimSpace(input), "\n\n") {
		lines := strings.Split(dump, "\n")
		if !strings.Contains(lines[0], "scanner") {
			panic("expected scanner dump start, but got: " + lines[0])
		}
		scannerId := utils.AllInts(lines[0])[0]
		if scannerId != i {
			panic(fmt.Sprintf("dumps out of order; expected %d but got %d", i, scannerId))
		}
		beacons := make(beacons)
		for _, line := range lines[1:] {
			coords := utils.AllInts(line)
			beacons[threed.Vector{X: coords[0], Y: coords[1], Z: coords[2]}] = true
		}

		scanners = append(scanners, &beacons)
	}

	return scanners
}
