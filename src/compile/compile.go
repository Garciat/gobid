package compile

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/garciat/gobid/check"
	. "github.com/garciat/gobid/common"
	"github.com/garciat/gobid/parse"
	"github.com/garciat/gobid/tree"
)

type CompilationUnit struct {
	GOROOT   string
	GOPATH   string
	FileSet  *token.FileSet
	Root     ImportPath
	Packages map[ImportPath]*Package
	Files    map[string]*tree.FileDef
}

type Package struct {
	ImportPath ImportPath
	Path       string
	Name       string
	Files      []*tree.FileDef
	Checker    *check.Checker
}

func NewPackage(path string, ip ImportPath) *Package {
	return &Package{
		ImportPath: ip,
		Path:       path,
		Name:       ip.PackageName(),
		Files:      nil,
		Checker:    check.NewChecker(),
	}
}

func (p *Package) AddFile(file *tree.FileDef) {
	if p.Name != file.Package {
		panic(fmt.Errorf("package name mismatch: %v != %v", p.Name, file.Package))
	}
	p.Files = append(p.Files, file)
}

func NewCompilationUnit(root ImportPath) *CompilationUnit {
	return &CompilationUnit{
		GOROOT:  GetGOROOT(),
		GOPATH:  GetGOPATH(),
		FileSet: token.NewFileSet(),
		Root:    root,
		Packages: map[ImportPath]*Package{
			root: NewPackage("", root),
		},
		Files: map[string]*tree.FileDef{},
	}
}

func GetGOROOT() string {
	out, err := exec.Command("go", "env", "GOROOT").Output()
	if err != nil {
		panic(fmt.Errorf("failed to get GOROOT: %w", err))
	}
	return strings.TrimSpace(string(out))
}

func GetGOPATH() string {
	out, err := exec.Command("go", "env", "GOPATH").Output()
	if err != nil {
		panic(fmt.Errorf("failed to get GOPATH: %w", err))
	}
	return strings.TrimSpace(string(out))
}

func (u *CompilationUnit) AddFile(path string) {
	fast, err := parser.ParseFile(u.FileSet, path, nil, parser.ParseComments)
	if err != nil {
		panic(err)
	}
	file := u.ParseFileAST(path, fast)
	u.Packages[u.Root].AddFile(file)
	u.LoadFile(file)
}

func (u *CompilationUnit) ParseFileAST(path string, fast *ast.File) *tree.FileDef {
	fmt.Printf("=== ParseFileAST(%v) ===\n", path)
	return parse.ReadFile(path, fast)
}

func (u *CompilationUnit) LoadFile(file *tree.FileDef) {
	if _, ok := u.Files[file.Path]; ok {
		return
	}

	fmt.Printf("=== LoadFile(%v) ===\n", file.Path)

	u.Files[file.Path] = file

	for _, decl := range file.Imports {
		u.LoadPackage(decl)
	}
}

func (u *CompilationUnit) FindImport(ip ImportPath) (string, error) {
	path := ip.String()

	candidates := []string{
		filepath.Join(u.GOROOT, "src", path),
		filepath.Join(u.GOROOT, "src", "vendor", path),
	}

	for _, candidate := range candidates {
		if _, err := os.Stat(candidate); err == nil {
			return candidate, nil
		}
	}

	return "", fmt.Errorf("import not found: %v", path)
}

func (u *CompilationUnit) LoadPackage(decl tree.ImportDecl) {
	if decl.ImportPath == "C" {
		fmt.Printf("skipping C import\n")
		return
	}

	if decl.ImportPath == "unsafe" {
		fmt.Printf("skipping unsafe import\n")
		return
	}

	if _, ok := u.Packages[decl.ImportPath]; ok {
		return
	}

	fmt.Printf("=== LoadImport(%v) ===\n", decl.ImportPath)

	path, err := u.FindImport(decl.ImportPath)
	if err != nil {
		panic(fmt.Errorf("failed to find import: %w", err))
	}

	packages, err := parser.ParseDir(u.FileSet, path, nil, parser.ParseComments)
	if err != nil {
		panic(fmt.Errorf("failed to parse GOROOT: %w", err))
	}

	p := NewPackage(path, decl.ImportPath)

	for _, pkg := range packages {
		if pkg.Name != decl.ImportPath.PackageName() {
			fmt.Printf("skipping package %v\n", pkg.Name)
			continue
		}
		for path, f := range pkg.Files {
			if strings.HasSuffix(path, "_test.go") {
				fmt.Printf("skipping test file %v\n", path)
				continue
			}
			p.AddFile(u.ParseFileAST(path, f))
		}
	}

	u.Packages[decl.ImportPath] = p

	for _, file := range p.Files {
		u.LoadFile(file)
	}
}

func (u *CompilationUnit) Compile() {
	for _, file := range u.Files {
		c := check.NewChecker()

		for _, decl := range file.Decls {
			switch decl := decl.(type) {
			case *tree.ConstDecl:

			case *tree.VarDecl:
				// TODO
			default:
				c.DefineDecl(decl)
			}
		}
	}
}
