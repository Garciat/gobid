package check

import (
	"fmt"
	. "github.com/garciat/gobid/common"
	"github.com/garciat/gobid/source"
	"github.com/garciat/gobid/tree"
)

type Checker struct {
	Fresh *int

	Packages       map[ImportPath]*source.Package
	PackageSymbols map[ImportPath]*VarContext

	TyCtx    *TypeContext
	VarCtx   *VarContext
	Builtins *VarContext

	CurFn *tree.Signature
	CurTy *tree.TypeDecl
}

func NewChecker(packages map[ImportPath]*source.Package) *Checker {
	return &Checker{
		Fresh:    Ptr(0),
		Packages: packages,
		PackageSymbols: map[ImportPath]*VarContext{
			"unsafe": MakeUnsafePackage(),
		},
		TyCtx:    &TypeContext{},
		VarCtx:   NewVarContext(),
		Builtins: MakeBuiltins(),
	}
}

func (c *Checker) Copy() *Checker {
	return &Checker{
		Fresh:          c.Fresh,
		Packages:       c.Packages,
		PackageSymbols: c.PackageSymbols,
		TyCtx:          c.TyCtx,
		VarCtx:         c.VarCtx,
		Builtins:       c.Builtins,
		CurFn:          c.CurFn,
		CurTy:          c.CurTy,
	}
}

func (c *Checker) Run() {
	packages := SortPackages(c.Packages)

	packageScopes := make(map[*source.Package]*Checker, len(packages))
	fileScopes := make(map[*source.FileDef]*Checker, len(packages)) // minimum size :shrug:

	for _, pkg := range packages {
		if pkg.ImportPath == "unsafe" {
			continue // TODO should not be here
		}

		packageScopes[pkg] = c.BeginScope(ScopeKindPackage)
		for _, file := range pkg.Files {
			fileScopes[file] = packageScopes[pkg].BeginScope(ScopeKindFile)

			for _, decl := range file.Decls {
				switch decl := decl.(type) {
				case *tree.ImportDecl:
					fileScopes[file].DefineImportDecl(decl)
				}
			}
		}

		c.PackageSymbols[pkg.ImportPath] = packageScopes[pkg].VarCtx // TODO seems unprincipled
	}

	packageNameResults := make(map[*source.Package]NameResolutionResult, len(packages))

	for _, pkg := range packages {
		if pkg.ImportPath == "unsafe" {
			continue // TODO should not be here
		}

		packageNameResults[pkg] = ResolvePackageNames(pkg)
	}

	for _, pkg := range packages {
		if pkg.ImportPath == "unsafe" {
			continue // TODO should not be here
		}

		// Keep track which file owns which declaration
		declFile := map[tree.Decl]*source.FileDef{}
		for _, file := range pkg.Files {
			for _, decl := range file.Decls {
				declFile[decl] = file
			}
		}

		seen := Set[tree.Decl]{}

		fmt.Printf("=== Loading Package (%v) ===\n", pkg.ImportPath)
		for _, decl := range packageNameResults[pkg].SortedDecls {
			switch decl := decl.(type) {
			case *tree.ImportDecl:
				// done earlier
			default:
				if seen.Contains(decl) {
					fmt.Printf("DUPLICATE DECL %v\n", decl) // TODO (P0) should not happen!!!
					continue
				}
				fmt.Println(declFile[decl].Path)
				scope := fileScopes[declFile[decl]]
				scope.DefineTopLevelDecl(decl)
				seen.Add(decl)
			}
		}

		for receiverName, methodsByName := range packageNameResults[pkg].Methods {
			packageScope := packageScopes[pkg]
			receiverTy, ok := packageScope.ResolveType(&tree.TypeName{Name: receiverName}).(*tree.NamedType)
			if !ok {
				panic(fmt.Errorf("not a named type: %v", receiverName))
			}
			receiverTy.Methods = methodsByName
		}
	}

	for _, pkg := range packages {
		fmt.Printf("=== Checking Package (%v) ===\n", pkg.ImportPath)
		for _, file := range pkg.Files {
			scope := fileScopes[file]
			scope.CheckFile(file)
		}
	}
}
