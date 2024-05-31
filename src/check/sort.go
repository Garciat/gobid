package check

import (
	"fmt"
	"github.com/garciat/gobid/algos"
	. "github.com/garciat/gobid/common"
	"github.com/garciat/gobid/source"
	"github.com/garciat/gobid/tree"
)

type DeclKind int

const (
	DeclKindConst DeclKind = iota
	DeclKindVar
)

func SortDeclarations(pkg *source.Package) []tree.Decl {
	fmt.Printf("=== SortDeclarations(%v) ===\n", pkg.ImportPath)

	var sortableDecls = map[Identifier]tree.Decl{}
	var declKinds = map[Identifier]DeclKind{}

	for _, file := range pkg.Files {
		for _, decl := range file.Decls {
			switch decl := decl.(type) {
			case *tree.ConstDecl:
				sortableDecls[decl.Name] = decl
				declKinds[decl.Name] = DeclKindConst
			case *tree.VarDecl:
				for _, name := range decl.Names {
					sortableDecls[name] = decl
					declKinds[name] = DeclKindVar
				}
			}
		}
	}

	declDeps := map[Identifier]map[Identifier]struct{}{}

	for _, file := range pkg.Files {
		for _, decl := range file.Decls {
			switch decl := decl.(type) {
			case *tree.VarDecl:
				for n, d := range VarDeclDependencies(declKinds, decl) {
					declDeps[n] = d
				}
			case *tree.ConstDecl:
				for n, d := range ConstDeclDependencies(declKinds, decl) {
					declDeps[n] = d
				}
			}
		}
	}

	cycle := algos.FindCycle(declDeps, func(deps map[Identifier]struct{}) map[Identifier]struct{} {
		return deps
	})
	if len(cycle) > 0 {
		panic(fmt.Errorf("cycle detected: %v", cycle))
	}

	empty := map[Identifier]struct{}{}

	sortedDecls := algos.TopologicalSort(sortableDecls, func(decl tree.Decl) map[Identifier]struct{} {
		switch decl := decl.(type) {
		case *tree.ConstDecl:
			return declDeps[decl.Name]
		case *tree.VarDecl:
			combo := map[Identifier]struct{}{}
			for _, name := range decl.Names {
				for dep := range declDeps[name] {
					combo[dep] = struct{}{}
				}
			}
			return combo
		default:
			return empty
		}
	})

	sortedDecls = algos.Uniq(sortedDecls)

	var finalDecls []tree.Decl
	for _, file := range pkg.Files {
		for _, decl := range file.Decls {
			switch decl.(type) {
			case *tree.ConstDecl, *tree.VarDecl:
				// skip
			default:
				finalDecls = append(finalDecls, decl)
			}
		}
	}

	return append(finalDecls, sortedDecls...)
}

func VarDeclDependencies(
	declKinds map[Identifier]DeclKind,
	decl *tree.VarDecl,
) map[Identifier]map[Identifier]struct{} {
	deps := map[Identifier]map[Identifier]struct{}{}

	refs := VarReferences(declKinds, decl.Expr, DeclKindVar, DeclKindConst)
	for _, name := range decl.Names {
		if name == IgnoreIdent {
			continue
		}
		deps[name] = refs
	}

	return deps
}

func ConstDeclDependencies(
	declKinds map[Identifier]DeclKind,
	decl *tree.ConstDecl,
) map[Identifier]map[Identifier]struct{} {
	deps := map[Identifier]map[Identifier]struct{}{
		decl.Name: VarReferences(declKinds, decl.Value, DeclKindConst),
	}
	return deps
}

func VarReferences(
	declKinds map[Identifier]DeclKind,
	expr tree.Expr,
	wanted ...DeclKind,
) map[Identifier]struct{} {
	references := map[Identifier]struct{}{}

	tree.WalkExpr(tree.ExprVisitorFunc(func(expr tree.Expr) {
		switch expr := expr.(type) {
		case *tree.NameExpr:
			if kind, ok := declKinds[expr.Name]; ok {
				for _, want := range wanted {
					if kind == want {
						references[expr.Name] = struct{}{}
					}
				}
			}
		}
	}), expr)

	return references
}
