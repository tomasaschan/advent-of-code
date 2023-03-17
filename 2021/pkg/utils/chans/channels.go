package chans

func Collect[T any](ch <-chan T) (items []T) {
	for item := range ch {
		items = append(items, item)
	}
	return
}
