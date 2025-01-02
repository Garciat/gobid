package main

import (
	"github.com/garciat/gobid/check"
	"github.com/garciat/gobid/compile"
	"io"
	"os"
)

func main() {
	unit := compile.NewCompilationUnit("main")
	for _, arg := range os.Args[1:] {
		switch arg {
		case "-v":
			*check.DebugAll = true
		case "-":
			stdin, err := io.ReadAll(os.Stdin)
			if err != nil {
				panic(err)
			}
			unit.AddSource("stdin.go", stdin)
		default:
			unit.AddFile(arg)
		}
	}
	unit.Compile()
}
