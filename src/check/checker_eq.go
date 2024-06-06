package check

import (
	"github.com/davecgh/go-spew/spew"
	"github.com/garciat/gobid/tree"
)

func (c *Checker) Identical(ty1, ty2 tree.Type) bool {
	// fmt.Printf("== Identical(%v, %v) ==\n", ty1, ty2)
	// TODO recursive types?
	switch ty1 := ty1.(type) {
	case *tree.NamedType:
		if ty2, ok := ty2.(*tree.NamedType); ok {
			return ty1.Name == ty2.Name
		}
		return false
	case *tree.TypeBuiltin:
		if ty2, ok := ty2.(*tree.TypeBuiltin); ok {
			return ty1.Name == ty2.Name
		}
		return false
	case *tree.TypeParam:
		if ty2, ok := ty2.(*tree.TypeParam); ok {
			return ty1.Name == ty2.Name
		}
		return false
	case *tree.InterfaceType:
		if ty2, ok := ty2.(*tree.InterfaceType); ok {
			if len(ty1.Methods) != len(ty2.Methods) {
				return false
			}
			if len(ty1.Constraints) != len(ty2.Constraints) {
				return false
			}
			panic("TODO")
		}
		return false
	case *tree.PointerType:
		if ty2, ok := ty2.(*tree.PointerType); ok {
			return c.Identical(c.ResolveType(ty1.ElemType), c.ResolveType(ty2.ElemType))
		}
		return false
	case *tree.TypeApplication:
		if ty2, ok := ty2.(*tree.TypeApplication); ok {
			if ty1.Type != ty2.Type {
				return false
			}
			if len(ty1.Args) != len(ty2.Args) {
				return false
			}
			for i, arg := range ty1.Args {
				if !c.Identical(c.ResolveType(arg), c.ResolveType(ty2.Args[i])) {
					return false
				}
			}
			return true
		}
		return false
	case *tree.ArrayType:
		if ty2, ok := ty2.(*tree.ArrayType); ok {
			if ty1.Len != ty2.Len {
				return false
			}
			return c.Identical(ty1.ElemType, ty2.ElemType)
		}
		return false
	case *tree.StructType:
		if ty2, ok := ty2.(*tree.StructType); ok {
			if len(ty1.Fields) != len(ty2.Fields) {
				return false
			}
			for i := range ty1.Fields {
				if ty1.Fields[i].Name != ty2.Fields[i].Name {
					return false
				}
				if !c.Identical(c.ResolveType(ty1.Fields[i].Type), c.ResolveType(ty2.Fields[i].Type)) {
					return false
				}
			}
			return true
		}
		return false
	case *tree.SliceType:
		if ty2, ok := ty2.(*tree.SliceType); ok {
			return c.Identical(c.ResolveType(ty1.ElemType), c.ResolveType(ty2.ElemType))
		}
		return false
	case *tree.MapType:
		if ty2, ok := ty2.(*tree.MapType); ok {
			if !c.Identical(c.ResolveType(ty1.KeyType), c.ResolveType(ty2.KeyType)) {
				return false
			}
			if !c.Identical(c.ResolveType(ty1.ValueType), c.ResolveType(ty2.ValueType)) {
				return false
			}
			return true
		}
		return false
	case *tree.FunctionType:
		if ty2, ok := ty2.(*tree.FunctionType); ok {
			return c.IdenticalFunctionTypes(ty1, ty2)
		}
		return false
	case *tree.UntypedConstantType:
		return false // TODO ???
	case *tree.NilType:
		return false // TODO ???
	default:
		spew.Dump(ty1, ty2)
		panic("unreachable")
	}
}

func (c *Checker) IdenticalFunctionTypes(ty1, ty2 *tree.FunctionType) bool {
	if len(ty1.Signature.TypeParams.Params) != 0 {
		panic("cannot compare type with type parameters")
	}
	if len(ty2.Signature.TypeParams.Params) != 0 {
		panic("cannot compare type with type parameters")
	}
	if len(ty1.Signature.Params.Params) != len(ty2.Signature.Params.Params) {
		return false
	}
	if len(ty1.Signature.Results.Params) != len(ty2.Signature.Results.Params) {
		return false
	}
	for i := range ty1.Signature.Params.Params {
		// TODO variadic
		par1 := ty1.Signature.Params.Params[i]
		par2 := ty2.Signature.Params.Params[i]
		if !c.Identical(c.ResolveType(par1.Type), c.ResolveType(par2.Type)) {
			return false
		}
	}
	for i := range ty1.Signature.Results.Params {
		par1 := ty1.Signature.Results.Params[i]
		par2 := ty2.Signature.Results.Params[i]
		if !c.Identical(c.ResolveType(par1.Type), c.ResolveType(par2.Type)) {
			return false
		}
	}
	return true
}
