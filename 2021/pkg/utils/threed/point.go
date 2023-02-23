package threed

type Point struct{ X, Y, Z int }

func (p Point) WithX(x int) Point {
	return Point{x, p.Y, p.Z}
}
func (p Point) WithY(y int) Point {
	return Point{p.X, y, p.Z}
}
func (p Point) WithZ(z int) Point {
	return Point{p.X, p.Y, z}
}
