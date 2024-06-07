package main

import (
	"github.com/garciat/gobid/compile"
)

func main() {
	unit := compile.NewCompilationUnit("main")
	unit.AddFile("../examples/example.go")
	unit.Compile()
}
