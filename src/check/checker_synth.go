package check

import (
	"fmt"
	"github.com/davecgh/go-spew/spew"
	"github.com/garciat/gobid/common"
	"github.com/garciat/gobid/tree"
	"reflect"
)

func (c *Checker) Synth(expr tree.Expr) tree.Type {
	switch expr := expr.(type) {
	case *tree.NameExpr:
		return c.SynthNameExpr(expr)
	case *tree.ImportRef:
		return c.SynthImportRef(expr)
	case *tree.PackageNameExpr:
		return c.SynthPackageNameExpr(expr)
	case *tree.BinaryExpr:
		return c.SynthBinaryExpr(expr)
	case *tree.UnaryExpr:
		return c.SynthUnaryExpr(expr)
	case *tree.ConversionExpr:
		return c.SynthConversionExpr(expr)
	case *tree.SelectorExpr:
		return c.SynthSelectorExpr(expr)
	case *tree.IndexExpr:
		return c.SynthIndexExpr(expr)
	case *tree.TypeAssertionExpr:
		return c.SynthTypeAssertionExpr(expr)
	case *tree.CallExpr:
		return c.SynthCallExpr(expr)
	case *tree.LiteralExpr:
		return c.SynthLiteralExpr(expr)
	case *tree.TypeExpr:
		return &tree.TypeOfType{Type: c.ResolveType(expr.Type)}
	case *tree.CompositeLitExpr:
		return c.SynthCompositeLitExpr(expr)
	case *tree.SliceExpr:
		return c.SynthSliceExpr(expr)
	case *tree.FuncLitExpr:
		return c.SynthFuncLitExpr(expr)
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func (c *Checker) SynthNameExpr(expr *tree.NameExpr) tree.Type {
	return c.Lookup(expr.Name)
}

func (c *Checker) SynthImportRef(expr *tree.ImportRef) tree.Type {
	return &tree.ImportType{ImportPath: expr.ImportPath}
}

func (c *Checker) SynthPackageNameExpr(expr *tree.PackageNameExpr) tree.Type {
	pkg, ok := c.PackageSymbols[expr.Path]
	if !ok {
		panic(fmt.Errorf("package not loaded: %v", expr.Path))
	}

	ty, ok := pkg.Lookup(expr.Name)
	if !ok {
		panic(fmt.Errorf("package %v has no symbol %v", expr.Path, expr.Name))
	}

	return ty
}

func (c *Checker) SynthBinaryExpr(expr *tree.BinaryExpr) tree.Type {
	switch expr.Op {
	case tree.BinaryOpEq, tree.BinaryOpNeq, tree.BinaryOpLt, tree.BinaryOpLte, tree.BinaryOpGt, tree.BinaryOpGte:
		// TODO check comparable
		leftTy := c.Synth(expr.Left)
		rightTy := c.Synth(expr.Right)
		c.CheckEqualTypes(leftTy, rightTy)
		return c.BuiltinType("bool")
	case tree.BinaryOpAdd, tree.BinaryOpSub, tree.BinaryOpMul, tree.BinaryOpQuo, tree.BinaryOpRem:
		// TODO check numeric?
		leftTy := c.Synth(expr.Left)
		rightTy := c.Synth(expr.Right)
		c.CheckEqualTypes(leftTy, rightTy)
		return leftTy
	case tree.BinaryOpLAnd, tree.BinaryOpLOr:
		c.CheckExpr(expr.Left, c.BuiltinType("bool"))
		c.CheckExpr(expr.Right, c.BuiltinType("bool"))
		return c.BuiltinType("bool")
	case tree.BinaryOpAnd, tree.BinaryOpOr, tree.BinaryOpXor, tree.BinaryOpAndNot, tree.BinaryOpShl, tree.BinaryOpShr:
		// TODO check numeric?
		leftTy := c.Synth(expr.Left)
		rightTy := c.Synth(expr.Right)
		c.CheckEqualTypes(leftTy, rightTy)
		return leftTy
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func (c *Checker) SynthUnaryExpr(expr *tree.UnaryExpr) tree.Type {
	ty := c.Synth(expr.Expr)
	switch expr.Op {
	case tree.UnaryOpPos, tree.UnaryOpNeg:
		// TODO check numeric?
		return ty
	case tree.UnaryOpNot:
		c.CheckExpr(expr.Expr, c.BuiltinType("bool"))
		return c.BuiltinType("bool")
	case tree.UnaryOpAddr:
		return &tree.PointerType{ElemType: ty}
	case tree.UnaryOpDeref:
		switch ty := ty.(type) {
		case *tree.PointerType:
			return c.ResolveType(ty.ElemType)
		case *tree.TypeOfType:
			return &tree.TypeOfType{Type: &tree.PointerType{ElemType: ty.Type}}
		default:
			spew.Dump(expr)
			panic(fmt.Errorf("cannot dereference type %v", ty))
		}
	case tree.UnaryOpBitNot:
		// TODO check integral type
		return ty
	case tree.UnaryOpArrow:
		switch ty := c.Under(ty).(type) {
		case *tree.ChannelType:
			return c.ResolveType(ty.ElemType)
		}
		panic(fmt.Errorf("receive on non-chan type %v", ty))
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func (c *Checker) SynthSelectorExpr(expr *tree.SelectorExpr) tree.Type {
	return c.DoSelect(c.Synth(expr.Expr), expr.Sel)
}

func (c *Checker) DoSelect(exprTy tree.Type, sel common.Identifier) tree.Type {
	switch ty := c.ResolveValue(exprTy).(type) {
	case *tree.ImportType:
		return c.PackageLookup(ty.ImportPath, sel)
	}

	switch ty := exprTy.(type) {
	case *tree.TypeOfType:
		switch ty := ty.Type.(type) {
		case *tree.ImportRef:
			return c.PackageLookup(ty.ImportPath, sel)
		}
	}

	checkTy := c.ResolveType(exprTy)

	switch ty := checkTy.(type) {
	case *tree.PointerType:
		checkTy = c.ResolveType(ty.ElemType)
	}

	switch ty := checkTy.(type) {
	case *tree.TypeParam:
		// TODO move this to GetMemberType
		if ty.Bound != nil {
			set := c.InterfaceTypeSet(ty.Bound)
			for _, m := range set.Methods {
				if m.Name == sel {
					return m.Type
				}
			}
			if len(set.Terms) == 1 {
				return c.DoSelect(c.ResolveType(set.Terms[0].Type), sel)
			}
		}
	}

	ty, err := c.GetMemberType(checkTy, sel)
	if err != nil {
		spew.Dump(exprTy)
		panic(err)
	}

	return ty
}

func (c *Checker) SynthIndexExpr(expr *tree.IndexExpr) tree.Type {
	exprTy := c.Synth(expr.Expr)

	switch ty := exprTy.(type) {
	case *tree.TypeOfType:
		var tyArgs []tree.Type
		for _, arg := range expr.Indices {
			tyArgs = append(tyArgs, c.Synth(arg))
		}
		return c.TypeApplication(&tree.TypeApplication{
			Type: ty.Type,
			Args: tyArgs,
		})
	}

	if len(expr.Indices) != 1 {
		panic("index expression with more than one index")
	}

	var index = expr.Indices[0]

	switch ty := exprTy.(type) {
	case *tree.PointerType:
		exprTy = c.ResolveType(ty.ElemType)
	}

	var indexTy, resultTy tree.Type

	ok := c.IsLike(exprTy, func(ty tree.Type) bool {
		switch ty := ty.(type) {
		case *tree.BuiltinType:
			if ty.IsString() {
				indexTy = tree.UntypedConstantIntType
				resultTy = tree.BuiltinTypeByte
				return true
			}
			return false
		default:
			if c.IsByteArray(ty) {
				indexTy = tree.UntypedConstantIntType
				resultTy = tree.BuiltinTypeByte
				return true
			}
			return false
		}
	})
	if ok {
		c.CheckExpr(index, indexTy)
		return resultTy
	}

	switch exprTy := c.Under(exprTy).(type) {
	case *tree.FunctionType:
		panic("unexpected function type (should be handled by CallExpr)")
	case *tree.SliceType:
		indexTy = tree.UntypedConstantIntType
		resultTy = c.ResolveType(exprTy.ElemType)
	case *tree.MapType:
		indexTy = c.ResolveType(exprTy.KeyType)
		resultTy = c.ResolveType(exprTy.ValueType)
	case *tree.ArrayType:
		indexTy = tree.UntypedConstantIntType
		resultTy = c.ResolveType(exprTy.ElemType)
	case *tree.BuiltinType:
		if exprTy.IsString() {
			indexTy = tree.UntypedConstantIntType
			resultTy = tree.BuiltinTypeByte
		}
	case *tree.UntypedConstantType:
		if exprTy.IsAssignableTo(tree.BuiltinTypeString) {
			indexTy = tree.UntypedConstantIntType
			resultTy = tree.BuiltinTypeByte
		}
	}

	if resultTy == nil || indexTy == nil {
		spew.Dump(reflect.TypeOf(exprTy))
		panic(fmt.Errorf("cannot index type %v", exprTy))
	}

	c.CheckAssignableTo(c.Synth(index), indexTy)

	return resultTy
}

func (c *Checker) SynthTypeAssertionExpr(expr *tree.TypeAssertionExpr) tree.Type {
	var fromTy *tree.InterfaceType

	switch ty := c.Synth(expr.Expr).(type) {
	case *tree.InterfaceType:
		fromTy = ty
	default:
		panic(fmt.Errorf("cannot type assert non-interface type %v", ty))
	}

	var toTy = c.ResolveType(expr.Type)

	c.CheckAssignableTo(toTy, fromTy)

	return toTy
}

func (c *Checker) SynthSliceExpr(expr *tree.SliceExpr) tree.Type {
	exprTy := c.ResolveType(c.Synth(expr.Expr))

	switch ty := c.Under(exprTy).(type) {
	case *tree.PointerType:
		exprTy = c.ResolveType(ty.ElemType)
	}

	var resultTy tree.Type

	switch exprTy := c.Under(exprTy).(type) {
	case *tree.SliceType:
		resultTy = exprTy
	case *tree.BuiltinType:
		if exprTy.IsString() {
			resultTy = tree.BuiltinTypeString
		}
	case *tree.ArrayType:
		resultTy = &tree.SliceType{ElemType: c.ResolveType(exprTy.ElemType)}
	case *tree.UntypedConstantType:
		if exprTy.IsString() {
			resultTy = tree.BuiltinTypeString
		}
	}

	if resultTy == nil {
		ok := c.IsLike(exprTy, func(ty tree.Type) bool {
			switch ty := ty.(type) {
			case *tree.BuiltinType:
				if ty.IsString() {
					resultTy = exprTy
					return true
				}
				return false
			default:
				if c.IsByteArray(ty) {
					resultTy = exprTy
					return true
				}
				return false
			}
		})
		if !ok {
			spew.Dump(exprTy)
			panic(fmt.Errorf("cannot slice type %v", exprTy))
		}
		common.Assert(resultTy != nil, "BUG")
	}

	if expr.Low != nil {
		c.CheckAssignableTo(c.Synth(expr.Low), tree.BuiltinTypeInt)
	}
	if expr.High != nil {
		c.CheckAssignableTo(c.Synth(expr.High), tree.BuiltinTypeInt)
	}
	if expr.Max != nil {
		c.CheckAssignableTo(c.Synth(expr.Max), tree.BuiltinTypeInt)
	}

	return resultTy
}

func (c *Checker) SynthCallExpr(expr *tree.CallExpr) tree.Type {
	var funcTy *tree.FunctionType
	var typeArgs []tree.Type
	if index, ok := expr.Func.(*tree.IndexExpr); ok {
		if gen, ok := c.Synth(index.Expr).(*tree.FunctionType); ok {
			funcTy = gen
			for _, arg := range index.Indices {
				typeArgs = append(typeArgs, c.Synth(arg))
			}
		}
	}
	if funcTy == nil {
		switch ty := c.Synth(expr.Func).(type) {
		case *tree.FunctionType:
			funcTy = ty
		case *tree.BuiltinFunctionType:
			return c.SynthBuiltinFunctionCall(ty, expr)
		case *tree.SliceType:
			if len(expr.Args) != 1 {
				panic("conversion without exactly one argument")
			}
			switch elemType := c.Under(ty.ElemType).(type) {
			case *tree.BuiltinType:
				if elemType.Tag == tree.BuiltinTypeTagByte {
					c.CheckAssignableTo(c.Synth(expr.Args[0]), tree.BuiltinTypeString)
					return ty
				}
			}
			panic("TODO")
		case *tree.BuiltinType:
			if len(expr.Args) != 1 {
				panic("conversion without exactly one argument")
			}
			return c.SynthConversionExpr(&tree.ConversionExpr{Expr: expr.Args[0], Type: c.ResolveType(ty)})
		case *tree.NamedType:
			if len(expr.Args) != 1 {
				panic("conversion without exactly one argument")
			}
			return c.SynthConversionExpr(&tree.ConversionExpr{Expr: expr.Args[0], Type: c.ResolveType(ty)})
		case *tree.TypeOfType:
			if len(expr.Args) != 1 {
				panic("conversion without exactly one argument")
			}
			return c.SynthConversionExpr(&tree.ConversionExpr{Expr: expr.Args[0], Type: c.ResolveType(ty.Type)})
		default:
			spew.Dump(ty)
			panic("not a function")
		}
	}
	variadicParam, variadicIndex := funcTy.Signature.GetVariadicParam()
	if len(expr.Args) != len(funcTy.Signature.Params.Params) && !expr.Ellipsis {
		if variadicIndex != -1 && len(expr.Args) < len(funcTy.Signature.Params.Params) {
			panic("not enough arguments")
		}
	}
	if len(typeArgs) > len(funcTy.Signature.TypeParams.Params) {
		panic("too many type arguments")
	}

	funcTy = c.InstantiateFunctionType(funcTy, typeArgs)

	for i, arg := range expr.Args {
		var param *tree.ParameterDecl
		if variadicIndex != -1 && i >= variadicIndex {
			param = variadicParam
		} else {
			param = funcTy.Signature.Params.Params[i]
		}

		argTy := c.Synth(arg)

		actualParamTy := param.Type
		if variadicIndex == i && expr.Ellipsis {
			actualParamTy = &tree.SliceType{ElemType: actualParamTy}
		}

		switch param.Type.(type) {
		case *tree.FreeTypeVar:
			c.CheckEqualTypes(argTy, actualParamTy)
		default:
			c.CheckAssignableTo(argTy, actualParamTy)
		}
	}

	var returns []tree.Type
	for _, result := range funcTy.Signature.Results.Params {
		returns = append(returns, result.Type)
	}

	switch len(returns) {
	case 0:
		return tree.TheVoidType
	case 1:
		return returns[0]
	default:
		return &tree.TupleType{Elems: returns}
	}
}

func (c *Checker) SynthFuncLitExpr(expr *tree.FuncLitExpr) tree.Type {
	return &tree.FunctionType{Signature: expr.Signature}
}

func (c *Checker) SynthConversionExpr(conv *tree.ConversionExpr) tree.Type {
	exprTy := c.ResolveType(c.Synth(conv.Expr))
	targetTy := c.ResolveType(conv.Type)

	switch exprTy.(type) {
	case *tree.NilType:
		if c.AcceptsNil(targetTy) {
			return targetTy
		}
	case *tree.TypeParam:
		c.CheckAssignableTo(exprTy, targetTy) // delegate to unification?
		return targetTy
	}

	if c.IsStringLike(exprTy) && c.IsByteArray(targetTy) {
		return targetTy
	}

	switch targetTy := targetTy.(type) {
	case *tree.BuiltinType:
		return c.SynthBuiltinConversion(conv.Expr, targetTy)
	case *tree.NamedType:
		return c.SynthNamedConversion(conv.Expr, targetTy)
	case *tree.PointerType:
		switch exprTy := exprTy.(type) {
		case *tree.BuiltinType:
			if exprTy.Tag == tree.BuiltinTypeTagUnsafePointer {
				return targetTy
			}
		}
		panic(fmt.Errorf("cannot convert %v to %v", exprTy, targetTy))
	default:
		spew.Dump(conv)
		panic("unreachable")
	}
}

func (c *Checker) SynthBuiltinConversion(expr tree.Expr, targetTy *tree.BuiltinType) tree.Type {
	exprTy := c.ResolveType(c.Synth(expr))

	switch exprTy := c.Under(exprTy).(type) {
	case *tree.UntypedConstantType:
		if exprTy.IsAssignableTo(targetTy) {
			return targetTy
		}
	case *tree.BuiltinType:
		if exprTy.IsConversibleTo(targetTy) {
			return targetTy
		}
	}

	switch targetTy.Tag {
	case tree.BuiltinTypeTagUnsafePointer:
		switch exprTy := c.Under(exprTy).(type) {
		case *tree.PointerType:
			return targetTy
		case *tree.BuiltinType:
			if exprTy.Tag == tree.BuiltinTypeTagUnsafePointer {
				return targetTy
			}
			if exprTy.Tag == tree.BuiltinTypeTagUintptr {
				return targetTy
			}
		}
	case tree.BuiltinTypeTagString:
		if c.IsByteArray(exprTy) {
			return targetTy
		}
	}

	panic(fmt.Errorf("cannot convert %v to %v", exprTy, targetTy))
}

func (c *Checker) SynthNamedConversion(expr tree.Expr, targetTy *tree.NamedType) tree.Type {
	exprTy := c.Synth(expr)
	switch exprTy := exprTy.(type) {
	case *tree.NamedType:
		if exprTy.SameType(targetTy) {
			return targetTy
		}
	}
	c.CheckAssignableTo(exprTy, targetTy.Definition)
	return targetTy
}

func (c *Checker) SynthLiteralExpr(expr *tree.LiteralExpr) tree.Type {
	switch expr.Literal.(type) {
	case *tree.LiteralInt:
		return &tree.UntypedConstantType{Kind: tree.UntypedConstantInt}
	case *tree.LiteralBool:
		return &tree.UntypedConstantType{Kind: tree.UntypedConstantBool}
	case *tree.LiteralString:
		return &tree.UntypedConstantType{Kind: tree.UntypedConstantString}
	case *tree.LiteralFloat:
		return &tree.UntypedConstantType{Kind: tree.UntypedConstantFloat}
	case *tree.LiteralRune:
		return &tree.UntypedConstantType{Kind: tree.UntypedConstantRune}
	default:
		panic("unreachable")
	}
}

func (c *Checker) SynthCompositeLitExpr(expr *tree.CompositeLitExpr) tree.Type {
	if expr.Type == nil {
		panic("composite literal without type")
	}
	return c.MakeCompositeLit(expr, c.ResolveType(expr.Type))
}

func (c *Checker) MakeCompositeLit(expr *tree.CompositeLitExpr, targetTy tree.Type) tree.Type {
	switch exprTy := c.Under(targetTy).(type) {
	case *tree.PointerType:
		elemTy := c.ResolveType(exprTy.ElemType)
		if _, ok := c.Under(elemTy).(*tree.PointerType); ok {
			panic("composite literal of double pointer type?")
		}
		return &tree.PointerType{ElemType: c.MakeCompositeLit(expr, elemTy)}
	case *tree.StructType:
		c.CheckCompositeLitStruct(expr, exprTy)
	case *tree.SliceType:
		c.CheckCompositeLitSlice(expr, exprTy)
	case *tree.MapType:
		c.CheckCompositeLitMap(expr, exprTy)
	case *tree.ArrayType:
		c.CheckCompositeLitArray(expr, exprTy)

		switch exprTy.Len.(type) {
		case *tree.EllipsisExpr:
			return &tree.ArrayType{
				ElemType: exprTy.ElemType,
				Len:      &tree.ConstIntExpr{Value: int64(len(expr.Elems))},
			}
		}
	default:
		spew.Dump(exprTy)
		panic("unreachable")
	}
	return targetTy
}

func (c *Checker) CheckCompositeLitStruct(expr *tree.CompositeLitExpr, structTy *tree.StructType) {
	if len(expr.Elems) == 0 {
		return
	}

	ordered := expr.Elems[0].Key == nil

	if ordered {
		if len(expr.Elems) != len(structTy.Fields) {
			panic("composite literal with wrong number of fields")
		}
		for i, elem := range expr.Elems {
			if elem.Key != nil {
				panic("composite literal with ordered fields")
			}
			field := structTy.Fields[i]
			c.CheckExpr(elem.Value, c.ResolveType(field.Type))
		}
	} else {
	elems:
		for _, elem := range expr.Elems {
			for _, field := range structTy.Fields {
				if elem.Key == nil {
					panic("composite literal with unordered fields")
				}

				var targetFieldName common.Identifier
				switch keyTy := elem.Key.(type) {
				case *tree.NameExpr:
					targetFieldName = keyTy.Name
				case *tree.TypeExpr:
					// TODO quirk of the name resolver
					switch keyTy := keyTy.Type.(type) {
					case *tree.PackageTypeName:
						targetFieldName = keyTy.Name
					case *tree.ImportRef:
						targetFieldName = keyTy.ImportDeclaredName
					default:
						spew.Dump(keyTy)
						panic("composite literal must use identifier as key name")
					}
				case *tree.ImportRef:
					// TODO quirk of the name resolver
					targetFieldName = keyTy.ImportDeclaredName
				case *tree.PackageNameExpr:
					// TODO quirk of the name resolver
					targetFieldName = keyTy.Name
				default:
					spew.Dump(keyTy)
					panic("composite literal must use identifier as key name")
				}

				if field.Name == targetFieldName {
					c.CheckExpr(elem.Value, c.ResolveType(field.Type))
					continue elems
				}
			}
			panic(fmt.Errorf("type %v has no field %v", structTy, elem.Key))
		}
	}
}

func (c *Checker) CheckCompositeLitSlice(expr *tree.CompositeLitExpr, sliceTy *tree.SliceType) {
	for _, elem := range expr.Elems {
		c.CheckExpr(elem.Value, c.ResolveType(sliceTy.ElemType))
	}
}

func (c *Checker) CheckCompositeLitMap(expr *tree.CompositeLitExpr, mapTy *tree.MapType) {
	for _, elem := range expr.Elems {
		c.CheckExpr(elem.Key, c.ResolveType(mapTy.KeyType))
		c.CheckExpr(elem.Value, c.ResolveType(mapTy.ValueType))
	}
}

func (c *Checker) CheckCompositeLitArray(expr *tree.CompositeLitExpr, arrayTy *tree.ArrayType) {
	for _, elem := range expr.Elems {
		c.CheckExpr(elem.Value, arrayTy.ElemType)
	}
}
