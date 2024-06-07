package common

import (
	"fmt"
	"runtime/debug"
)

func Try[T any](f func() T) (result T, err error, stack string) {
	defer func() {
		if r := recover(); r != nil {
			switch r := r.(type) {
			case error:
				err = r
			default:
				err = fmt.Errorf("%v", r)
			}
			stack = string(debug.Stack())
		}
	}()
	return f(), nil, ""
}
