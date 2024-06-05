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
		c.DefineMethodDecl(decl)
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
	//spew.Dump(decl)
	var exprTy tree.Type = c.Synth(decl.Value)
	var declTy tree.Type
	if decl.Type != nil {
		declTy = c.ResolveType(decl.Type)
		c.CheckAssignableTo(exprTy, declTy)
	} else {
		declTy = exprTy
	}
	c.DefineConstant(decl.Name, declTy, decl)
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

func (c *Checker) DefineMethodDecl(decl *tree.MethodDecl) {
	pointerReceiver := false

	receiverTy := c.ResolveType(decl.Receiver.Type)
	if pointerTy, ok := receiverTy.(*tree.PointerType); ok {
		receiverTy = c.ResolveType(pointerTy.BaseType)
		pointerReceiver = true
	}

	switch c.Under(receiverTy).(type) {
	case *tree.InterfaceType:
		panic("cannot define methods on interface types")
	}

	var methodHolder common.Identifier

	switch receiverTy := receiverTy.(type) {
	case *tree.NamedType:
		methodHolder = receiverTy.Name
	case *tree.TypeApplication:
		switch receiverTy := c.ResolveType(receiverTy.Type).(type) {
		case *tree.NamedType:
			methodHolder = receiverTy.Name
		case *tree.QualIdentifier:
			panic("cannot have package-qualified receiver type")
		default:
			panic("unreachable?")
		}
	default:
		spew.Dump(receiverTy)
		panic("TODO")
	}

	methodTy := &tree.MethodType{
		PointerReceiver: pointerReceiver,
		Type:            &tree.FunctionType{Signature: decl.Signature},
	}

	c.DefineMethod(methodHolder, decl.Name, methodTy)
}
