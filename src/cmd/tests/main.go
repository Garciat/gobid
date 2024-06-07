package main

import (
	"fmt"
	"github.com/garciat/gobid/common"
	"github.com/garciat/gobid/compile"
	"os"
	"path/filepath"
	"strings"
)

func main() {
	var testsPath = "../tests"
	entries, err := os.ReadDir(testsPath)
	if err != nil {
		panic(err)
	}

	for _, entry := range entries {
		switch {
		case entry.IsDir():
			testDir(testsPath, entry.Name())
		default:
			testSingleFile(testsPath, entry.Name())
		}
	}

	testSingleFile("../examples", "default.go")
}

func testDir(parent, name string) {
	path := filepath.Join(parent, name)

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

	_, err, stack := common.Try(func() int {
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
			failExpectedPass(name, err, stack)
		}
	default:
		panic(fmt.Errorf("unexpected file %s", name))
	}
}

func testSingleFile(parent, name string) {
	path := filepath.Join(parent, name)

	_, err, stack := common.Try(func() int {
		unit := compile.NewCompilationUnit("tests")
		unit.AddFile(path)
		unit.Compile()
		return 0
	})

	switch {
	case strings.HasPrefix(name, "fail_"):
		if err == nil {
			failExpectedError(name)
		}
	case strings.HasPrefix(name, "pass_"):
		fallthrough
	default:
		// eh, for the example files
		if err != nil {
			failExpectedPass(name, err, stack)
		}
	}
}

func failExpectedError(name string) {
	fmt.Printf("FAIL %s: expected error\n", name)
	os.Exit(1)
}

func failExpectedPass(name string, err error, stack string) {
	fmt.Printf("FAIL %s: unexpected error:\n%v\n", name, err)
	fmt.Printf("%s\n", dropStacks(stack, 3))
	os.Exit(1)
}

func dropStacks(stack string, n int) string {
	return strings.Join(strings.Split(stack, "\n")[1+n*2:], "\n")
}
