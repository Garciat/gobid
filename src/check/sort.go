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
				for _, elem := range decl.Elems {
					sortableDecls[elem.Name] = decl
					declKinds[elem.Name] = DeclKindConst
				}
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
			combo := map[Identifier]struct{}{}
			for _, elem := range decl.Elems {
				for dep := range declDeps[elem.Name] {
					combo[dep] = struct{}{}
				}
			}
			return combo
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

	if len(decl.Names) == len(decl.Exprs) {
		for i, name := range decl.Names {
			deps[name] = VarReferences(declKinds, decl.Exprs[i], DeclKindVar, DeclKindConst)
		}
	} else if len(decl.Names) > 1 && len(decl.Exprs) == 1 {
		refs := VarReferences(declKinds, decl.Exprs[0], DeclKindVar, DeclKindConst)
		for _, name := range decl.Names {
			if name == IgnoreIdent {
				continue
			}
			deps[name] = refs
		}
	}

	return deps
}

func ConstDeclDependencies(
	declKinds map[Identifier]DeclKind,
	decl *tree.ConstDecl,
) map[Identifier]map[Identifier]struct{} {
	deps := map[Identifier]map[Identifier]struct{}{}

	var carryIndex = -1
	for i, elem := range decl.Elems {
		deps[elem.Name] = map[Identifier]struct{}{}

		if elem.Type == nil {
			if elem.Value == nil {
				if carryIndex == -1 {
					panic("mising init expr for constant")
				}
				deps[elem.Name][decl.Elems[carryIndex].Name] = struct{}{}
			} else {
				deps[elem.Name] = VarReferences(declKinds, elem.Value, DeclKindConst)
				if ContainsIota(elem.Value) {
					carryIndex = i
				}
			}
		} else {
			carryIndex = -1
			if elem.Value != nil {
				deps[elem.Name] = VarReferences(declKinds, elem.Value, DeclKindConst)
				if ContainsIota(elem.Value) {
					carryIndex = i
				}
			} else {
				panic("mising init expr for constant")
			}
		}
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
