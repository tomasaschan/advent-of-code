package funcs

type FuncIntInt func(int) int

func MemoizeIntInt(f FuncIntInt) FuncIntInt {
	cache := map[int]int{}

	return func(i int) int {
		if j, ok := cache[i]; ok {
			return j
		}

		j := f(i)
		cache[i] = j
		return j
	}
}
