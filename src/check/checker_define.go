package check

import (
	"fmt"
	"github.com/davecgh/go-spew/spew"
	"github.com/garciat/gobid/common"
	"github.com/garciat/gobid/tree"
	"sync"
)

func (c *Checker) DefineTopLevelDecl(decl tree.Decl) {
	switch decl := decl.(type) {
	case *tree.ImportDecl:
		c.DefineImportDecl(decl)
	case *tree.ConstDecl:
		c.DefineConstDecl(decl)
	case *tree.TypeDecl:
		c.DefineTypeDecl(decl)
	case *tree.AliasDecl:
		c.DefineAliasDecl(decl)
	case *tree.VarDecl:
		c.DefineVarDecl(decl)
	case *tree.FunctionDecl:
		c.DefineFunctionDecl(decl)
	case *tree.MethodDecl:
		panic("unexpected")
	default:
		spew.Dump(decl)
		panic("unreachable")
	}
}

func (c *Checker) DefineDecl(decl tree.Decl) {
	switch decl := decl.(type) {
	case *tree.ConstDecl:
		c.DefineConstDecl(decl)
	case *tree.TypeDecl:
		c.DefineTypeDecl(decl)
	case *tree.AliasDecl:
		c.DefineAliasDecl(decl)
	case *tree.VarDecl:
		c.DefineVarDecl(decl)
	default:
		spew.Dump(decl)
		panic("unreachable")
	}
}

func (c *Checker) DefineImportDecl(decl *tree.ImportDecl) {
	c.DefineImport(decl)
}

func (c *Checker) DefineConstDecl(decl *tree.ConstDecl) {
	CheckerPrintf("DEFINING CONSTANT %v\n", decl.Name)

	if c.Ctx.ScopeKind == ScopeKindFile {
		common.Assert(c.Ctx.Parent.ScopeKind == ScopeKindPackage, "expected package scope")

		// Only evaluate constants in package scope
		value := c.EvaluateConstantExpr(decl, decl.Value)
		CheckerPrintf("EVALUATED %v = %v\n", decl.Name, value)

		valueTy := value.Type()

		var declTy tree.Type
		if decl.Type != nil {
			declTy = c.ResolveType(decl.Type)
			c.CheckAssignableTo(valueTy, declTy)
		} else {
			declTy = value.Type().DefaultType()
		}

		c.Ctx.Parent.Def(decl.Name, &tree.ConstValueType{Value: value, Type: declTy})
	} else {
		valueTy := c.Synth(decl.Value)
		var declTy tree.Type
		if decl.Type != nil {
			declTy = c.ResolveType(decl.Type)
			c.CheckAssignableTo(valueTy, declTy)
		} else {
			declTy = valueTy
		}
		c.Ctx.Def(decl.Name, declTy) // TODO mark as constant? (cannot reassign)
	}
}

func (c *Checker) DefineTypeDecl(decl *tree.TypeDecl) {
	var ty tree.Type
	if len(decl.TypeParams.Params) > 0 {
		ty = &tree.NamedType{
			Package:    c.CurPkg.ImportPath,
			Name:       decl.Name,
			Definition: &tree.GenericType{TypeParams: decl.TypeParams, Type: decl.Type},
		}
	} else {
		ty = &tree.NamedType{
			Name:       decl.Name,
			Definition: decl.Type,
		}
	}
	c.DefineType(decl.Name, ty)
}

func (c *Checker) DefineAliasDecl(decl *tree.AliasDecl) {
	c.DefineType(decl.Name, decl.Type)
}

func (c *Checker) DefineVarDecl(decl *tree.VarDecl) {
	if len(decl.Names) == 1 && decl.Expr != nil {
		for _, name := range decl.Names {
			ty := c.Synth(decl.Expr)
			switch ty.(type) {
			case *tree.VoidType:
				panic(fmt.Errorf("cannot use void as value"))
			case *tree.TupleType:
				panic(fmt.Errorf("multiple-value return in single-value context"))
			}
			if decl.Type != nil {
				declTy := c.ResolveType(decl.Type)
				c.CheckAssignableTo(ty, declTy)
				c.DefineValue(name, declTy)
			} else {
				t := c.NewFreeType()
				c.CheckEqualTypes(t, ty)
				c.DefineValue(name, ty)
			}
		}
	} else if len(decl.Names) > 1 && decl.Expr != nil {
		tupleTy := sync.OnceValue(func() *tree.TupleType {
			exprTy := c.Synth(decl.Expr)
			var tupleTy *tree.TupleType
			switch ty := exprTy.(type) {
			case *tree.VoidType:
				panic(fmt.Errorf("cannot use void as value"))
			case *tree.TupleType:
				tupleTy = ty
			default:
				panic(fmt.Errorf("expected tuple type, got %v", ty))
			}
			if len(tupleTy.Elems) != len(decl.Names) {
				panic(fmt.Errorf("assignment mismatch: %v variables but RHS returns %v values", len(decl.Names), len(tupleTy.Elems)))
			}
			return tupleTy
		})
		for i, name := range decl.Names {
			elemTy := c.ResolveType(tupleTy().Elems[i])
			if decl.Type != nil {
				declTy := c.ResolveType(decl.Type)
				c.CheckAssignableTo(elemTy, declTy)
				c.DefineValue(name, declTy)
			} else {
				c.DefineValue(name, elemTy)
			}
		}
	} else if len(decl.Names) > 0 && decl.Expr == nil {
		if decl.Type == nil {
			panic("variable declaration without type or initializing expression")
		}
		for _, name := range decl.Names {
			c.DefineValue(name, decl.Type)
		}
	} else {
		spew.Dump(decl)
		panic("unreachable?")
	}
}

func (c *Checker) DefineFunctionDecl(decl *tree.FunctionDecl) {
	c.DefineFunction(decl.Name, &tree.FunctionType{Signature: decl.Signature})
}
