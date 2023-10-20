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

func Max(i int, j int) int {
	if i > j {
		return i
	} else {
		return j
	}
}

func ArithmeticSum(a1, d, n int) int {
	return n * (a1 + (a1 + n*d)) / 2
}

func Pow(b, n int) int {
	if n == 0 {
		return 1
	}
	if n == 1 {
		return b
	}
	y := Pow(b, n/2)
	if n%2 == 0 {
		return y * y
	}
	return b * y * y

}
