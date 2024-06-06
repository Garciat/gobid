package main

import (
	"fmt"
	"github.com/garciat/gobid/compile"
	"os"
	"path/filepath"
	"strings"
)

func main() {
	testsPath := "../tests"

	entries, err := os.ReadDir(testsPath)
	if err != nil {
		panic(err)
	}

	for _, entry := range entries {
		if entry.IsDir() {
			continue
		}

		name := entry.Name()
		path := filepath.Join(testsPath, entry.Name())

		_, err := Try(func() int {
			unit := compile.NewCompilationUnit("tests")
			unit.AddFile(path)
			unit.Compile()
			return 0
		})

		switch {
		case strings.HasPrefix(name, "fail_"):
			if err == nil {
				panic(fmt.Errorf("expected error for %s", name))
			}
		case strings.HasPrefix(name, "pass_"):
			if err != nil {
				panic(fmt.Errorf("unexpected error for %s: %w", name, err))
			}
		default:
			panic(fmt.Errorf("unexpected file %s", name))
		}
	}
}

func Try[T any](f func() T) (result T, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = r.(error)
		}
	}()
	return f(), nil
}
