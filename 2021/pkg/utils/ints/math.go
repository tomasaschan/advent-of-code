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

func ArithmeticSum(a1, d, n int) int {
	return n * (a1 + (a1 + n*d)) / 2
}
