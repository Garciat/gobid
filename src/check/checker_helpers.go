package check

import (
	"github.com/davecgh/go-spew/spew"
	"github.com/garciat/gobid/tree"
)

func (c *Checker) IsPointerType(ty tree.Type) bool {
	switch ty.(type) {
	case *tree.PointerType:
		return true
	default:
		return false
	}
}

func (c *Checker) IsTypeParam(ty tree.Type) bool {
	switch ty.(type) {
	case *tree.TypeParam:
		return true
	default:
		return false
	}
}

func (c *Checker) IsConcreteType(ty tree.Type) bool {
	switch ty := ty.(type) {
	case *tree.NamedType:
		return c.IsConcreteType(c.ResolveType(ty.Type))
	case *tree.TypeOfType:
		return c.IsConcreteType(ty.Type)
	case *tree.TypeBuiltin:
		return true
	case *tree.InterfaceType:
		return false
	case *tree.TypeParam:
		return false
	case *tree.PointerType:
		return true
	case *tree.SliceType:
		return true
	case *tree.ArrayType:
		return true
	case *tree.StructType:
		return true
	case *tree.TypeApplication:
		return c.IsConcreteType(c.TypeApplication(ty))
	case *tree.UntypedConstantType:
		return true
	case *tree.FunctionType:
		return true
	default:
		spew.Dump(ty)
		panic("unreachable")
	}
}

func (c *Checker) IsNumeric(ty tree.Type) bool {
	switch ty := c.Under(ty).(type) {
	case *tree.TypeBuiltin:
		return ty.IsNumeric
	case *tree.UntypedConstantType:
		return ty.IsNumeric()
	default:
		return false
	}
}

func (c *Checker) IsLike(ty tree.Type, pred func(ty tree.Type) bool) bool {
	switch argTy := c.Under(ty).(type) {
	case *tree.TypeParam:
		tyset := c.InterfaceTypeSet(argTy.Bound)
		if tyset.Universe {
			return false
		}
		for _, t := range tyset.Types {
			if !pred(t) {
				return false
			}
		}
		return true
	default:
		return pred(argTy)
	}
}

func (c *Checker) IsByteArray(ty tree.Type) bool {
	return c.IsLike(ty, func(ty tree.Type) bool {
		switch ty := ty.(type) {
		case *tree.SliceType:
			return c.Identical(c.ResolveType(ty.ElemType), c.BuiltinType("byte"))
		default:
			return false
		}
	})
}

func (c *Checker) IsStringLike(ty tree.Type) bool {
	return c.IsLike(ty, func(ty tree.Type) bool {
		switch ty := ty.(type) {
		case *tree.TypeBuiltin:
			return ty.Name.Value == "string"
		case *tree.UntypedConstantType:
			return ty.IsCompatible("string")
		default:
			return false
		}
	})
}

func (c *Checker) HasLen(ty tree.Type) bool {
	return c.IsLike(ty, func(ty tree.Type) bool {
		switch ty := ty.(type) {
		case *tree.SliceType:
		case *tree.ArrayType:
		case *tree.MapType:
		case *tree.ChannelType:
		case *tree.TypeBuiltin:
			return ty.Name.Value == "string"
		case *tree.UntypedConstantType:
			return ty.IsCompatible("string")
		default:
			return false
		}
		return true
	})
}

func (c *Checker) UntypedDefaultType(ty *tree.UntypedConstantType) tree.Type {
	switch ty.Kind {
	case tree.UntypedConstantInt:
		return c.BuiltinType("int")
	case tree.UntypedConstantBool:
		return c.BuiltinType("bool")
	case tree.UntypedConstantString:
		return c.BuiltinType("string")
	case tree.UntypedConstantFloat:
		return c.BuiltinType("float64")
	case tree.UntypedConstantRune:
		return c.BuiltinType("rune")
	default:
		panic("unreachable")
	}
}
