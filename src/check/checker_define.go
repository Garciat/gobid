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
	fmt.Printf("DEFINING CONSTANT %v\n", decl.Name)

	if c.VarCtx.ScopeKind == ScopeKindFile {
		common.Assert(c.VarCtx.Parent.ScopeKind == ScopeKindPackage, "expected package scope")

		// Only evaluate constants in package scope
		value := c.EvaluateConstantExpr(decl, decl.Value)
		fmt.Printf("EVALUATED %v = %v\n", decl.Name, value)

		valueTy := value.Type()

		var declTy tree.Type
		if decl.Type != nil {
			declTy = c.ResolveType(decl.Type)
			c.CheckAssignableTo(valueTy, declTy)
		} else {
			declTy = value.Type().DefaultType()
		}

		c.VarCtx.Parent.Def(decl.Name, &tree.ConstValueType{Value: value, Type: declTy})
	} else {
		valueTy := c.Synth(decl.Value)
		var declTy tree.Type
		if decl.Type != nil {
			declTy = c.ResolveType(decl.Type)
			c.CheckAssignableTo(valueTy, declTy)
		} else {
			declTy = valueTy
		}
		c.VarCtx.Def(decl.Name, declTy) // TODO mark as constant? (cannot reassign)
	}
}

func (c *Checker) DefineTypeDecl(decl *tree.TypeDecl) {
	var ty tree.Type
	if len(decl.TypeParams.Params) > 0 {
		ty = &tree.NamedType{Name: decl.Name, Type: &tree.GenericType{TypeParams: decl.TypeParams, Type: decl.Type}}
	} else {
		ty = &tree.NamedType{Name: decl.Name, Type: decl.Type}
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
			if _, ok := ty.(*tree.TupleType); ok {
				panic("cannot use tuple as value")
			}
			if decl.Type != nil {
				declTy := c.ResolveType(decl.Type)
				c.CheckAssignableTo(ty, declTy)
				c.DefineValue(name, declTy)
			} else {
				c.DefineValue(name, ty)
			}
		}
	} else if len(decl.Names) > 1 && decl.Expr != nil {
		tupleTy := sync.OnceValue(func() *tree.TupleType {
			exprTy := c.Synth(decl.Expr)
			tupleTy, ok := exprTy.(*tree.TupleType)
			if !ok {
				panic("multiple-value return in single-value context")
			}
			if len(tupleTy.Elems) != len(decl.Names) {
				panic(fmt.Sprintf("assignment mismatch: %v variables but RHS returns %v values", len(decl.Names), len(tupleTy.Elems)))
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
