package main

import (
	"fmt"
	"github.com/garciat/gobid/compile"
	"os"
	"path/filepath"
	"runtime/debug"
	"strings"
)

var testsPath = "../tests"

func main() {
	entries, err := os.ReadDir(testsPath)
	if err != nil {
		panic(err)
	}

	for _, entry := range entries {
		switch {
		case entry.IsDir():
			testDir(entry)
		default:
			testSingleFile(entry)
		}
	}
}

func testDir(entry os.DirEntry) {
	name := entry.Name()
	path := filepath.Join(testsPath, entry.Name())

	entries, err := os.ReadDir(path)
	if err != nil {
		panic(err)
	}

	var files []string
	for _, entry := range entries {
		if !entry.IsDir() {
			files = append(files, filepath.Join(path, entry.Name()))
		}
	}

	_, err, stack := Try(func() int {
		unit := compile.NewCompilationUnit("tests", path)
		for _, file := range files {
			unit.AddFile(file)
		}
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
			fmt.Printf("unexpected error for %s: %v", name, err)
			fmt.Printf("%s\n", stack)
			panic("fail")
		}
	default:
		panic(fmt.Errorf("unexpected file %s", name))
	}
}

func testSingleFile(entry os.DirEntry) {
	name := entry.Name()
	path := filepath.Join(testsPath, entry.Name())

	_, err, stack := Try(func() int {
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
			fmt.Printf("unexpected error for %s: %v", name, err)
			fmt.Printf("%s\n", stack)
			panic("fail")
		}
	default:
		panic(fmt.Errorf("unexpected file %s", name))
	}
}

func Try[T any](f func() T) (result T, err error, stack string) {
	defer func() {
		if r := recover(); r != nil {
			err = r.(error)
			stack = string(debug.Stack())
		}
	}()
	return f(), nil, ""
}
