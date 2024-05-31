package main

import (
	"github.com/garciat/gobid"
)

func main() {
	unit := gobid.NewCompilationUnit("main")
	unit.AddFile("../example.go")
	unit.Compile()
}
