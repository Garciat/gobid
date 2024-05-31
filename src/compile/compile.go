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

	for _, ip := range file.Imports {
		pkg.AddDependency(ip)

		if _, ok := u.Packages[ip]; !ok {
			u.Packages[ip] = NewPackage(ip)
			for _, f := range u.ReadPackageFiles(ip) {
				u.LoadFile(ip, f)
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
	u.CheckPackageDeclarations()
	u.CheckPackageCycles()

	for _, pkg := range u.Packages {
		for _, file := range pkg.Files {
			_ = file
		}
	}
}

func (u *CompilationUnit) CheckPackageDeclarations() {
	fmt.Printf("=== CheckPackageDeclarations ===\n")
	for _, pkg := range u.Packages {
		names := make(map[string]struct{})
		for _, file := range pkg.Files {
			names[file.PackageName] = struct{}{}
		}
		if len(names) > 1 {
			panic(fmt.Errorf("package %v contains multiple package declarations: %v", pkg.ImportPath, names))
		}
	}

}

func (u *CompilationUnit) CheckPackageCycles() {
	fmt.Printf("=== CheckPackageCycles ===\n")

	visited := make(map[ImportPath]struct{})

	var visit func(ImportPath, ImportPath, []ImportPath)

	visit = func(start, ip ImportPath, path []ImportPath) {
		visited[ip] = struct{}{}
		path = append(path, ip)

		pkg := u.Packages[ip]
		for dep := range pkg.Dependencies {
			if _, ok := visited[dep]; ok {
				for i, p := range path {
					if p == dep {
						panic(fmt.Errorf("package cycle detected: %v", path[i:]))
						return
					}
				}
			} else {
				visit(start, dep, path)
			}
		}

		delete(visited, ip)
	}

	for ip := range u.Packages {
		if _, ok := visited[ip]; !ok {
			visit(ip, ip, nil)
		}
	}
}
