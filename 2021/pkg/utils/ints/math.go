package ints

func Abs(i int) int {
	q := i >> 63
	return (i + q) ^ q
}

func Min(i int, j int) int {
	if i < j {
		return i
	} else {
		return j
	}
}
