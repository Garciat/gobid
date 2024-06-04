package compile

import (
	"fmt"
	"github.com/garciat/gobid/check"
	. "github.com/garciat/gobid/common"
	"github.com/garciat/gobid/files"
	"github.com/garciat/gobid/gocompiler"
	"github.com/garciat/gobid/parse"
	"github.com/garciat/gobid/source"
	"go/build/constraint"
	"strings"
)

type BuildTags = map[string]bool

type CompilationUnit struct {
	finder    files.Finder
	parser    parse.Parser
	Root      ImportPath
	BuildTags BuildTags
	Packages  map[ImportPath]*source.Package
}

func NewCompilationUnit(
	root ImportPath,
) *CompilationUnit {
	return &CompilationUnit{
		finder: files.NewFinder(),
		parser: parse.NewParser(),
		Root:   root,
		BuildTags: BuildTags{
			"arm64":  true,
			"darwin": true,
			"unix":   true,
		},
		Packages: map[ImportPath]*source.Package{
			root: source.NewPackage(root),
		},
	}
}

func (u *CompilationUnit) AddFile(path string) {
	u.LoadFile(u.Root, u.parser.ParseFile(path))
}

func (u *CompilationUnit) LoadFile(target ImportPath, file *source.FileDef) {
	pkg, ok := u.Packages[target]
	if !ok {
		panic(fmt.Errorf("package not found: %v", target))
	}

	if strings.HasSuffix(file.Path, "_test.go") {
		//fmt.Printf("skipping test file %v\n", file.Path)
		return
	}

	if !u.EvalArchSuffix(file.Path) {
		//fmt.Printf("skipping file %v with arch suffix\n", file.Path)
		return
	}

	if !u.EvalBuildConstraint(file.BuildConstraint) {
		//fmt.Printf("skipping file %v with build constraint '%v'\n", file.Path, file.BuildConstraint)
		return
	}

	fmt.Printf("loading file %v\n", file.Path)

	pkg.AddFile(file)

	for _, ip := range file.Imports {
		pkg.AddDependency(ip)

		if _, ok := u.Packages[ip]; !ok {
			u.Packages[ip] = source.NewPackage(ip)
			for _, f := range u.ReadPackageFiles(ip) {
				u.LoadFile(ip, f)
			}
		}
	}
}

func (u *CompilationUnit) EvalBuildConstraint(bc string) bool {
	if bc == "" {
		return true
	}

	expr, err := constraint.Parse(bc)
	if err != nil {
		panic(err)
	}

	return expr.Eval(func(tag string) bool {
		_, ok := u.BuildTags[tag]
		return ok
	})
}

func (u *CompilationUnit) EvalArchSuffix(path string) bool {
	filename := path
	if i := strings.LastIndex(path, "/"); i != -1 {
		filename = path[i+1:]
	}
	return gocompiler.MatchFile(filename, u.BuildTags)
}

func (u *CompilationUnit) ReadPackageFiles(ip ImportPath) []*source.FileDef {
	if ip == "C" {
		fmt.Printf("skipping C import\n")
		return nil
	}

	if ip == "unsafe" {
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

	check.NewChecker(u.Packages).Run()
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
