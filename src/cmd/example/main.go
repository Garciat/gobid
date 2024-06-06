package main

import (
	"github.com/garciat/gobid/compile"
)

func main() {
	unit := compile.NewCompilationUnit("main")
	unit.AddFile("../examples/example.go")
	unit.Compile()
}

func Try[T any](f func() T) (result T, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = r.(error)
		}
	}()
	return f(), nil
}
