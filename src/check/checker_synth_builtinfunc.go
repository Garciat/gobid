package check

import (
	"fmt"
	"github.com/davecgh/go-spew/spew"
	"github.com/garciat/gobid/tree"
)

func (c *Checker) SynthBuiltinFunctionCall(f *tree.BuiltinFunctionType, expr *tree.CallExpr) tree.Type {
	switch f.Name {
	case "new":
		return c.SynthBuiltinNewCall(expr)
	case "make":
		return c.SynthBuiltinMakeCall(expr)
	case "append":
		return c.SynthBuiltinAppendCall(expr)
	case "len":
		return c.SynthBuiltinLenCall(expr)
	case "cap":
		return c.SynthBuiltinCapCall(expr)
	case "close":
		return c.SynthBuiltinCloseCall(expr)
	case "panic":
		return c.SynthBuiltinPanicCall(expr)
	case "print":
		return c.SynthBuiltinPrintCall(expr)
	case "println":
		return c.SynthBuiltinPrintlnCall(expr)
	case "copy":
		return c.SynthBuiltinCopyCall(expr)
	case "clear":
		return c.SynthBuiltinClearCall(expr)
	case "real":
		return c.SynthBuiltinRealCall(expr)
	case "imag":
		return c.SynthBuiltinImagCall(expr)
	default:
		spew.Dump(f)
		panic("unreachable")
	}
}

func (c *Checker) SynthBuiltinNewCall(expr *tree.CallExpr) tree.Type {
	if len(expr.Args) != 1 {
		panic("builtin new() takes exactly one argument")
	}
	argTy, ok := c.Synth(expr.Args[0]).(*tree.TypeOfType)
	if !ok {
		panic("new() with non-type argument")
	}
	return &tree.PointerType{ElemType: argTy.Type}
}

func (c *Checker) SynthBuiltinMakeCall(expr *tree.CallExpr) tree.Type {
	if len(expr.Args) == 0 {
		panic("builtin make() takes at least one argument")
	}
	argTy, ok := c.Synth(expr.Args[0]).(*tree.TypeOfType)
	if !ok {
		panic("make() with non-type argument")
	}
	elemTy := argTy.Type
	switch elemTy.(type) {
	case *tree.SliceType:
	case *tree.MapType:
	case *tree.ChannelType:
	default:
		panic("make() with non-slice, non-map, non-channel type")
	}
	for _, arg := range expr.Args[1:] {
		c.CheckExpr(arg, c.BuiltinType("int"))
	}
	return elemTy
}

func (c *Checker) SynthBuiltinAppendCall(expr *tree.CallExpr) tree.Type {
	if len(expr.Args) < 2 {
		panic("builtin append() takes at least two arguments")
	}
	firstTy := c.Synth(expr.Args[0])
	sliceTy, ok := firstTy.(*tree.SliceType)
	if !ok {
		panic("append() with non-slice type")
	}
	for _, arg := range expr.Args[1:] {
		c.CheckAssignableTo(c.Synth(arg), sliceTy.ElemType)
	}
	return sliceTy
}

func (c *Checker) SynthBuiltinLenCall(expr *tree.CallExpr) tree.Type {
	if len(expr.Args) != 1 {
		panic("builtin len() takes exactly one argument")
	}

	argTy := c.Synth(expr.Args[0])

	if c.IsPointerToArrayType(c.ResolveType(argTy)) {
		return c.BuiltinType("int")
	}

	ok := c.IsLike(argTy, func(ty tree.Type) bool {
		switch ty := ty.(type) {
		case *tree.SliceType:
		case *tree.ArrayType:
		case *tree.MapType:
		case *tree.ChannelType:
		case *tree.BuiltinType:
			return ty.IsString()
		case *tree.UntypedConstantType:
			return ty.IsString()
		default:
			return false
		}
		return true
	})
	if !ok {
		panic(fmt.Sprintf("len() on incompatible type %v", argTy))
	}

	return c.BuiltinType("int")
}

func (c *Checker) SynthBuiltinCapCall(expr *tree.CallExpr) tree.Type {
	if len(expr.Args) != 1 {
		panic("builtin len() takes exactly one argument")
	}

	argTy := c.Synth(expr.Args[0])

	if c.IsPointerToArrayType(argTy) {
		return c.BuiltinType("int")
	}

	ok := c.IsLike(argTy, func(ty tree.Type) bool {
		switch ty.(type) {
		case *tree.ArrayType:
		case *tree.SliceType:
		case *tree.ChannelType:
		default:
			return false
		}
		return true
	})
	if !ok {
		panic(fmt.Sprintf("cap() on incompatible type %v", argTy))
	}

	return c.BuiltinType("int")
}

func (c *Checker) SynthBuiltinCloseCall(expr *tree.CallExpr) tree.Type {
	if len(expr.Args) != 1 {
		panic("builtin close() takes exactly one argument")
	}

	ty := c.Synth(expr.Args[0])

	ok := c.IsLike(ty, func(ty tree.Type) bool {
		switch ty := ty.(type) {
		case *tree.ChannelType:
			return ty.Dir == tree.ChannelDirSend || ty.Dir == tree.ChannelDirBoth
		default:
			return false
		}
	})
	if !ok {
		panic(fmt.Sprintf("close() on incompatible type %v", ty))
	}

	return &tree.VoidType{}
}

func (c *Checker) SynthBuiltinPanicCall(expr *tree.CallExpr) tree.Type {
	if len(expr.Args) != 1 {
		panic("builtin panic() takes exactly one argument")
	}
	c.CheckExpr(expr.Args[0], tree.EmptyInterface())
	return &tree.BottomType{}
}

func (c *Checker) SynthBuiltinPrintCall(expr *tree.CallExpr) tree.Type {
	for _, arg := range expr.Args {
		c.Synth(arg)
	}
	return &tree.VoidType{}
}

func (c *Checker) SynthBuiltinPrintlnCall(expr *tree.CallExpr) tree.Type {
	for _, arg := range expr.Args {
		c.Synth(arg)
	}
	return &tree.VoidType{}
}

func (c *Checker) SynthBuiltinCopyCall(expr *tree.CallExpr) tree.Type {
	if len(expr.Args) != 2 {
		panic("builtin copy() takes exactly two arguments")
	}

	ty1 := c.Synth(expr.Args[0])
	ty2 := c.Synth(expr.Args[1])

	if c.IsByteArray(ty1) && c.IsStringLike(ty2) {
		return c.BuiltinType("int")
	}

	var elemTy1, elemTy2 tree.Type

	switch ty1 := c.Under(ty1).(type) {
	case *tree.SliceType:
		elemTy1 = ty1.ElemType
	default:
		panic(fmt.Sprintf("copy() with non-slice type %v", ty1))
	}

	switch ty2 := c.Under(ty2).(type) {
	case *tree.SliceType:
		elemTy2 = ty2.ElemType
	default:
		panic(fmt.Sprintf("copy() with non-slice type %v", ty2))
	}

	c.CheckEqualTypes(elemTy1, elemTy2)

	return c.BuiltinType("int")
}

func (c *Checker) SynthBuiltinClearCall(expr *tree.CallExpr) tree.Type {
	if len(expr.Args) != 1 {
		panic("builtin clear() takes exactly one argument")
	}

	ty := c.Synth(expr.Args[0])

	ok := c.IsLike(ty, func(ty tree.Type) bool {
		switch ty.(type) {
		case *tree.SliceType:
		case *tree.MapType:
		default:
			return false
		}
		return true
	})
	if !ok {
		panic(fmt.Sprintf("clear() on incompatible type %v", ty))
	}

	return &tree.VoidType{}
}

func (c *Checker) SynthBuiltinRealCall(expr *tree.CallExpr) tree.Type {
	if len(expr.Args) != 1 {
		panic("builtin real() takes exactly one argument")
	}

	argTy := c.Synth(expr.Args[0])

	ok := c.IsLike(argTy, func(ty tree.Type) bool {
		switch ty := ty.(type) {
		case *tree.BuiltinType:
			return ty.IsComplex()
		case *tree.UntypedConstantType:
			return ty.IsComplex()
		default:
			return false
		}
	})
	if !ok {
		panic(fmt.Sprintf("real() on incompatible type %v", argTy))
	}

	return c.BuiltinType("float64")
}

func (c *Checker) SynthBuiltinImagCall(expr *tree.CallExpr) tree.Type {
	if len(expr.Args) != 1 {
		panic("builtin imag() takes exactly one argument")
	}

	argTy := c.Synth(expr.Args[0])

	ok := c.IsLike(argTy, func(ty tree.Type) bool {
		switch ty := ty.(type) {
		case *tree.BuiltinType:
			return ty.IsComplex()
		case *tree.UntypedConstantType:
			return ty.IsComplex()
		default:
			return false
		}
	})
	if !ok {
		panic(fmt.Sprintf("imag() on incompatible type %v", argTy))
	}

	return c.BuiltinType("float64")
}
