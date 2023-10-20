package dec19

import (
	"github.com/tomasaschan/advent-of-code-2021/pkg/utils/threed"
)

type beacons map[threed.Vector]bool

type transformation struct {
	translation threed.Vector
	rotation    threed.Rotation
}

func (t *transformation) transform(v *threed.Vector) *threed.Vector {
	return t.translation.Add(t.rotation.Apply(v))
}

func (bs *beacons) extend(scan *beacons) (*transformation, bool) {
	transform, overlaps := relative_orientation(bs, scan)
	if !overlaps {
		return nil, false
	}

	for seen := range *scan {
		transformed := transform.transform(&seen)
		(*bs)[*transformed] = true
	}
	return transform, true
}

func relative_orientation(a *beacons, b *beacons) (*transformation, bool) {
	for _, rot := range threed.AllRotations {
		if translation, overlaps := overlaps(a, b, rot); overlaps {
			return &transformation{*translation, rot}, true
		}
	}
	return nil, false
}

func overlaps(a *beacons, b *beacons, rotation threed.Rotation) (*threed.Vector, bool) {
	matches := map[threed.Vector]int{}

	for p := range *a {
		for q := range *b {
			d := p.Subtract(rotation.Apply(&q))
			if c, ok := matches[*d]; ok {
				matches[*d] = c + 1
			} else {
				matches[*d] = 1
			}
		}
	}
	mx := 0
	for d, count := range matches {
		if count > mx {
			mx = count
		}
		if count >= 12 {
			return &d, true
		}
	}

	return nil, false
}
