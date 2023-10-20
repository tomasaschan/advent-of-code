package dec20

func A(input string) int {
	return solve(input, 2)
}

func B(input string) int {
	return solve(input, 50)
}

func solve(input string, n int) int {
	algorithm, image := parse(input)

	for i := 0; i < n; i++ {
		image = image.enhance(algorithm)
	}

	return image.brightPixels()
}
