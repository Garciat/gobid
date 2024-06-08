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
		return c.IsConcreteType(c.ResolveType(ty.Definition))
	case *tree.TypeOfType:
		return c.IsConcreteType(ty.Type)
	case *tree.BuiltinType:
		return true
	case *tree.InterfaceType:
		return false
	case *tree.TypeParam:
		return false
	case *tree.FreeTypeVar:
		return false
	case *tree.PointerType:
		return true
	case *tree.SliceType:
		return true
	case *tree.ArrayType:
		return true
	case *tree.MapType:
		return true
	case *tree.StructType:
		return true
	case *tree.TypeApplication:
		return c.IsConcreteType(c.TypeApplication(ty))
	case *tree.UntypedConstantType:
		return true
	case *tree.FunctionType:
		return true
	case *tree.ChannelType:
		return true
	default:
		spew.Dump(ty)
		panic("unreachable")
	}
}

func (c *Checker) IsNumeric(ty tree.Type) bool {
	switch ty := c.Under(ty).(type) {
	case *tree.BuiltinType:
		return ty.IsNumeric()
	case *tree.UntypedConstantType:
		return ty.IsNumeric()
	default:
		return false
	}
}

func (c *Checker) IsPointerToArrayType(ty tree.Type) bool {
	switch ty := c.Under(ty).(type) {
	case *tree.PointerType:
		_, ok := c.Under(ty.ElemType).(*tree.ArrayType)
		return ok
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
		for _, t := range tyset.Terms {
			if !pred(t.Type) {
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
		case *tree.BuiltinType:
			return ty.IsString()
		case *tree.UntypedConstantType:
			return ty.IsString()
		default:
			return false
		}
	})
}

func (c *Checker) AcceptsNil(ty tree.Type) bool {
	return c.IsLike(ty, func(ty tree.Type) bool {
		switch ty := ty.(type) {
		case *tree.PointerType:
			return true
		case *tree.ChannelType:
			return true
		case *tree.FunctionType:
			return true
		case *tree.InterfaceType:
			return true
		case *tree.MapType:
			return true
		case *tree.SliceType:
			return true
		case *tree.BuiltinType:
			return ty.Tag == tree.BuiltinTypeTagUnsafePointer
		default:
			return false
		}
	})
}

func (c *Checker) IsAny(inter *tree.InterfaceType) bool {
	typeset := c.InterfaceTypeSet(inter)
	return typeset.Universe && len(typeset.Terms) == 0 && len(typeset.Methods) == 0
}
