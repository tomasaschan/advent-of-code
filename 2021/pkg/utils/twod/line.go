package twod

type Line struct {
	Start Vector
	End   Vector
}

func (l *Line) Dx() int {
	if l.Start.X < l.End.X {
		return 1
	} else if l.Start.X > l.End.X {
		return -1
	} else {
		return 0
	}
}
func (l *Line) Dy() int {
	if l.Start.Y < l.End.Y {
		return 1
	} else if l.Start.Y > l.End.Y {
		return -1
	} else {
		return 0
	}
}

func (l *Line) IsDiagonal() bool {
	return l.Start.X != l.End.X && l.Start.Y != l.End.Y
}

func (l *Line) Points() []Vector {
	result := []Vector{l.Start}

	p := l.Start
	for p != l.End {
		p.X += l.Dx()
		p.Y += l.Dy()
		result = append(result, p)
	}

	return result
}
