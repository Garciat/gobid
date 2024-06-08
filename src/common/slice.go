package common

func PopBack[T any](s []T) (T, []T) {
	return s[len(s)-1], s[:len(s)-1]
}

func PushFront[T any](s []T, x T) []T {
	return append([]T{x}, s...)
}
