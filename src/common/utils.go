package common

import "fmt"

func Assert(cond bool, msg string) {
	if !cond {
		panic(msg)
	}
}

func Ptr[T any](x T) *T {
	return &x
}

func Try[T any](f func() T) (out T, err error) {
	done := make(chan struct{})
	go func() {
		defer func() {
			if r := recover(); r != nil {
				err = fmt.Errorf("%v", r)
				close(done)
			}
		}()
		out = f()
		close(done)
	}()
	<-done
	return
}
