package tests

func example1[T int|float32](x T) T {}

var _ = example1("hello")
