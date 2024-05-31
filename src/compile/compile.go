package compile

import (
	"fmt"
	. "github.com/garciat/gobid/common"
	"github.com/garciat/gobid/files"
	"github.com/garciat/gobid/parse"
	"github.com/garciat/gobid/tree"
)

type CompilationUnit struct {
	finder   files.Finder
	parser   parse.Parser
	Root     ImportPath
	Packages map[ImportPath]*Package
}

func NewCompilationUnit(
	root ImportPath,
) *CompilationUnit {
	return &CompilationUnit{
		finder: files.NewFinder(),
		parser: parse.NewParser(),
		Root:   root,
		Packages: map[ImportPath]*Package{
			root: NewPackage(root),
		},
	}
}

func (u *CompilationUnit) AddFile(path string) {
	u.LoadFile(u.Root, u.parser.ParseFile(path))
}

func (u *CompilationUnit) LoadFile(target ImportPath, file *tree.FileDef) {
	pkg, ok := u.Packages[target]
	if !ok {
		panic(fmt.Errorf("package not found: %v", target))
	}

	pkg.AddFile(file)

	for _, decl := range file.Imports {
		pkg.AddDependency(decl.ImportPath)

		if _, ok := u.Packages[decl.ImportPath]; !ok {
			u.Packages[decl.ImportPath] = NewPackage(decl.ImportPath)
			for _, f := range u.ReadPackageFiles(decl.ImportPath) {
				u.LoadFile(decl.ImportPath, f)
			}
		}
	}
}

func (u *CompilationUnit) ReadPackageFiles(ip ImportPath) []*tree.FileDef {
	if ip == "C" {
		fmt.Printf("skipping C import\n")
		return nil
	}

	if ip == "unsafe" {
		fmt.Printf("skipping unsafe import\n")
		return nil
	}

	path, err := u.finder.FindImport(ip)
	if err != nil {
		panic(err)
	}

	return u.parser.ParsePackage(path)
}

func (u *CompilationUnit) Compile() {
	for _, pkg := range u.Packages {
		for _, file := range pkg.Files {
			_ = file
		}
	}
}
