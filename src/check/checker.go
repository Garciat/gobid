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
	PackageSymbols map[ImportPath]*TypeContext

	Ctx      *TypeContext
	Builtins *TypeContext

	CurPkg  *source.Package
	CurFile *source.FileDef
	CurFn   *tree.Signature
	CurTy   *tree.TypeDecl
}

func NewChecker(packages map[ImportPath]*source.Package) *Checker {
	return &Checker{
		Fresh:    Ptr(0),
		Packages: packages,
		PackageSymbols: map[ImportPath]*TypeContext{
			"unsafe": MakeUnsafePackage(),
		},
		Ctx:      NewTypeContext(),
		Builtins: MakeBuiltins(),
	}
}

func (c *Checker) Copy() *Checker {
	return &Checker{
		Fresh:          c.Fresh,
		Packages:       c.Packages,
		PackageSymbols: c.PackageSymbols,
		Ctx:            c.Ctx,
		Builtins:       c.Builtins,
		CurPkg:         c.CurPkg,
		CurFile:        c.CurFile,
		CurFn:          c.CurFn,
		CurTy:          c.CurTy,
	}
}

func (c *Checker) Run() {
	packages := SortPackages(c.Packages)

	packageScopes := make(map[*source.Package]*Checker, len(packages))
	fileScopes := make(map[*source.FileDef]*Checker, len(packages)) // minimum size :shrug:

	for _, pkg := range packages {
		packageScopes[pkg] = c.BeginPackageScope(pkg)
		for _, file := range pkg.Files {
			fileScopes[file] = packageScopes[pkg].BeginFileScope(file)

			for _, decl := range file.Decls {
				switch decl := decl.(type) {
				case *tree.ImportDecl:
					fileScopes[file].DefineImportDecl(decl)
				}
			}
		}

		c.PackageSymbols[pkg.ImportPath] = packageScopes[pkg].Ctx
	}

	for _, pkg := range packages {
		// Keep track which file owns which declaration
		declFile := map[tree.Decl]*source.FileDef{}
		for _, file := range pkg.Files {
			for _, decl := range file.Decls {
				declFile[decl] = file
			}
		}

		nameResolution := ResolvePackageNames(pkg)

		seen := Set[tree.Decl]{}

		GeneralPrintf("=== Loading Package (%v) ===\n", pkg.ImportPath)
		for _, decl := range nameResolution.SortedDecls {
			switch decl := decl.(type) {
			case *tree.ImportDecl:
				// done earlier
			default:
				if seen.Contains(decl) {
					fmt.Printf("DUPLICATE DECL %v\n", decl) // TODO (P0) should not happen!!!
					continue
				}
				GeneralPrintf("%v\n", declFile[decl].Path)
				scope := fileScopes[declFile[decl]]
				scope.DefineTopLevelDecl(decl)
				seen.Add(decl)
			}
		}

		for receiverName, methodsByName := range nameResolution.Methods {
			packageScope := packageScopes[pkg]
			receiverTy, ok := packageScope.ResolveType(&tree.TypeName{Name: receiverName}).(*tree.NamedType)
			if !ok {
				panic(fmt.Errorf("not a named type: %v", receiverName))
			}
			receiverTy.Methods = methodsByName
		}
	}

	for _, pkg := range packages {
		GeneralPrintf("=== Checking Package (%v) ===\n", pkg.ImportPath)
		for _, file := range pkg.Files {
			GeneralPrintf("=== Checking File (%v) ===\n", file.Path)
			scope := fileScopes[file]
			scope.CheckFile(file)
		}
		GeneralPrintf("=== Verifying Package (%v) ===\n", pkg.ImportPath)
		packageScopes[pkg].Verify()
	}
}
