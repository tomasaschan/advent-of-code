package dec17

import (
	"math"
)

func x(dx0, t int) int {
	x := dx0 * (1 + dx0) / 2
	if t < dx0 {
		x -= (dx0 - t) * (1 + dx0 - t) / 2
	}
	return x
}

func y(dy0, t int) int {
	// return (dy0*(dy0+1) - (dy0-t)*(dy0-t+1)) / 2
	return dy0*t + (t-t*t)/2
}

// ASSUMPTION:
// Target area is completely below y=0,
// so we'll pass through ymax on the way in, and ymin on the way out,
// exactly once.
func tOfY(dy0, y int) float64 {
	// we return the larger root, since the smaller represents t<0
	x := float64(2*dy0 + 1)
	return x/2 + math.Sqrt(x*x/4-float64(y))
}

func tBoundsOfY(dy0, ymin, ymax int) (bool, int, int) {
	tmin := tOfY(dy0, ymax)
	tmax := tOfY(dy0, ymin)
	tmini := int(math.Floor(tmin))
	tmaxi := int(math.Ceil(tmax))
	hits, t0, t1 := false, -1, -1
	for t := tmini; t <= tmaxi; t++ {
		yt := y(dy0, t)
		if ymin <= yt && yt <= ymax {
			hits = true
			if t0 == -1 {
				t0 = t
			}
			t1 = t
		}
	}
	return hits, t0, t1
}

// ASSUMPTION:
// Target area is completely to the right of x=0, so dx0 must be positive.
// The boolean indicates whether we pass through x at all,
// which we won't if dx0 is too small in relation to x.
func tOfX(dx0, x int) (bool, float64) {
	xmax := dx0 * (dx0 + 1) / 2
	if xmax < x {
		return false, 0
	}

	// we return the smaller root, since the larger represents an unphysical horizontal rebound
	return true, (float64(dx0)+1)/2 - math.Sqrt(float64((dx0+1)*(dx0+1))/4-1)
}

func dxBounds(xmin, xmax int) (int, int) {
	dx0 := -1.0/2 + math.Sqrt(0.25+2.0*float64(xmin))
	return int(math.Floor(dx0)), xmax + 1
}
