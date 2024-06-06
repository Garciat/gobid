package tests

func Nil[T any]() T {
	var zero T
	return zero
}

type Vec[T any] struct{}

func MakeVec[T any]() Vec[T] {
	return Vec[T]{}
}

func Append[T any](v Vec[T], x T) {}

func ReadVec[T any](v Vec[T]) T {}

func Ptr[T any](x T) *T {
	return &x
}

func useVec() {
	v := MakeVec()
	Append(v, Nil())
	Append(v, Ptr(32))
}
