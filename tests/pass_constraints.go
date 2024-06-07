package tests

func example1[T int|float32](x T) T {}

var _ = example1(int(1))
var _ = example1(float32(1))

func example2[T interface{M()}](t T) T {}

var _ = example2[interface{M(); N()}](nil)
