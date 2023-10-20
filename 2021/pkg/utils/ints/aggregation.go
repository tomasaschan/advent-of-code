package ints

func Sum(is []int) int {
	s := 0
	for _, i := range is {
		s += i
	}
	return s
}

func CumSum(is []int) []int {
	result := make([]int, len(is))
	s := 0
	for i := range is {
		s += is[i]
		result[i] = s
	}
	return result
}
