package dec20

func A(input string) int {
	algorithm, image := parse(input)

	image = image.enhance(algorithm)
	image = image.enhance(algorithm)

	return image.brightPixels()
}

func B(input string) int {
	return 0
}
