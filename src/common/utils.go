package common

func Assert(cond bool, msg string) {
	if !cond {
		panic(msg)
	}
}

func Ptr[T any](x T) *T {
	return &x
}
