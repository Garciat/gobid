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
		spew.Dump(expr)
		panic("TODO")
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
		panic(fmt.Sprintf("package not loaded: %v", expr.Path))
	}

	ty, ok := pkg.Lookup(expr.Name)
	if !ok {
		panic(fmt.Sprintf("package %v has no symbol %v", expr.Path, expr.Name))
	}

	return ty
}

func (c *Checker) SynthBinaryExpr(expr *tree.BinaryExpr) tree.Type {
	switch expr.Op {
	case tree.BinaryOpEq, tree.BinaryOpNeq, tree.BinaryOpLt, tree.BinaryOpLte, tree.BinaryOpGt, tree.BinaryOpGte:
		// TODO check comparable
		leftTy := c.Synth(expr.Left)
		rightTy := c.Synth(expr.Right)
		c.TyCtx.AddEq(leftTy, rightTy)
		return c.BuiltinType("bool")
	case tree.BinaryOpAdd, tree.BinaryOpSub, tree.BinaryOpMul, tree.BinaryOpQuo, tree.BinaryOpRem:
		// TODO check numeric?
		leftTy := c.Synth(expr.Left)
		rightTy := c.Synth(expr.Right)
		c.TyCtx.AddEq(leftTy, rightTy)
		return leftTy
	case tree.BinaryOpLAnd, tree.BinaryOpLOr:
		c.CheckExpr(expr.Left, c.BuiltinType("bool"))
		c.CheckExpr(expr.Right, c.BuiltinType("bool"))
		return c.BuiltinType("bool")
	case tree.BinaryOpAnd, tree.BinaryOpOr, tree.BinaryOpXor, tree.BinaryOpAndNot, tree.BinaryOpShl, tree.BinaryOpShr:
		// TODO check numeric?
		leftTy := c.Synth(expr.Left)
		rightTy := c.Synth(expr.Right)
		c.TyCtx.AddEq(leftTy, rightTy)
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
			return ty.ElemType
		case *tree.TypeOfType:
			return &tree.TypeOfType{Type: &tree.PointerType{ElemType: ty.Type}}
		default:
			spew.Dump(expr)
			panic(fmt.Sprintf("cannot dereference %v of type %v", expr, ty))
		}
	case tree.UnaryOpBitNot:
		// TODO check numeric?
		return ty
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
			if len(set.Types) == 1 {
				return c.DoSelect(c.ResolveType(set.Types[0]), sel)
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
	case *tree.PointerType:
		exprTy = c.ResolveType(ty.ElemType)
	}

	var indexTy, resultTy tree.Type
	switch exprTy := c.Under(exprTy).(type) {
	case *tree.FunctionType:
		panic("unexpected function type (should be handled by CallExpr)")
	case *tree.SliceType:
		indexTy = tree.UntypedInt()
		resultTy = c.ResolveType(exprTy.ElemType)
	case *tree.MapType:
		indexTy = c.ResolveType(exprTy.KeyType)
		resultTy = c.ResolveType(exprTy.ValueType)
	case *tree.ArrayType:
		indexTy = tree.UntypedInt()
		resultTy = c.ResolveType(exprTy.ElemType)
	case *tree.TypeBuiltin:
		if exprTy.Name.Value == "string" {
			indexTy = tree.UntypedInt()
			resultTy = c.BuiltinType("byte")
		}
	case *tree.UntypedConstantType:
		if exprTy.IsCompatible("string") {
			indexTy = tree.UntypedInt()
			resultTy = c.BuiltinType("byte")
		}
	}

	if resultTy == nil || indexTy == nil {
		spew.Dump(reflect.TypeOf(exprTy))
		panic(fmt.Sprintf("cannot index type %v", exprTy))
	}

	return resultTy
}

func (c *Checker) SynthSliceExpr(expr *tree.SliceExpr) tree.Type {
	exprTy := c.Synth(expr.Expr)

	switch ty := c.Under(exprTy).(type) {
	case *tree.PointerType:
		exprTy = c.ResolveType(ty.ElemType)
	}

	var resultTy tree.Type
	switch exprTy := c.Under(exprTy).(type) {
	case *tree.SliceType:
		resultTy = exprTy
	case *tree.TypeBuiltin:
		if exprTy.Name.Value == "string" {
			resultTy = c.BuiltinType("string")
		}
	case *tree.ArrayType:
		resultTy = &tree.SliceType{ElemType: exprTy.ElemType}
	case *tree.UntypedConstantType:
		if exprTy.IsCompatible("string") {
			resultTy = c.BuiltinType("string")
		}
	}

	if resultTy == nil {
		spew.Dump(exprTy)
		panic(fmt.Sprintf("cannot slice type %v", exprTy))
	}

	if expr.Low != nil {
		c.CheckExpr(expr.Low, tree.UntypedInt())
	}
	if expr.High != nil {
		c.CheckExpr(expr.High, tree.UntypedInt())
	}
	if expr.Max != nil {
		c.CheckExpr(expr.Max, tree.UntypedInt())
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
			case *tree.TypeBuiltin:
				if elemType.Name.Value == "byte" {
					c.CheckAssignableTo(c.Synth(expr.Args[0]), c.BuiltinType("string"))
					return ty
				}
			}
			panic("TODO")
		case *tree.TypeBuiltin:
			if len(expr.Args) != 1 {
				panic("conversion without exactly one argument")
			}
			return c.SynthBuiltinConversion(expr.Args[0], ty)
		case *tree.TypeOfType:
			return c.SynthConversionExpr(&tree.ConversionExpr{Expr: expr.Func, Type: ty.Type})
		default:
			spew.Dump(ty)
			panic("not a function")
		}
	}
	var variadicParam *tree.ParameterDecl
	var variadicIndex int = -1
	if len(expr.Args) != len(funcTy.Signature.Params.Params) {
		variadicParam, variadicIndex = funcTy.Signature.GetVariadicParam()
		if variadicIndex != -1 && len(expr.Args) < len(funcTy.Signature.Params.Params) {
			panic("not enough arguments")
		}
	}
	if len(typeArgs) > len(funcTy.Signature.TypeParams.Params) {
		panic("too many type arguments")
	}

	subst := Subst{}
	for _, tyParam := range funcTy.Signature.TypeParams.Params {
		subst[tyParam.Name] = &tree.TypeParam{Name: c.FreshTypeName(), Bound: tyParam.Constraint}
	}
	funcTy = c.ApplySubst(funcTy, subst).(*tree.FunctionType)
	fmt.Printf("subst FunctionType: %v\n", funcTy)

	for i, tyArg := range typeArgs {
		tyParam := funcTy.Signature.TypeParams.Params[i]
		c.TyCtx.AddRelation(RelationSatisfies{Type: tyArg, Constraint: tyParam.Constraint})
		c.TyCtx.AddEq(tyArg, subst[tyParam.Name])
	}
	for _, tyParam := range funcTy.Signature.TypeParams.Params {
		ty := &tree.TypeParam{Name: tyParam.Name, Bound: tyParam.Constraint}
		c.TyCtx.AddRelation(RelationSubtype{Sub: ty, Super: tyParam.Constraint})
	}
	for i, arg := range expr.Args {
		var param *tree.ParameterDecl
		if variadicIndex != -1 && i >= variadicIndex {
			param = variadicParam
		} else {
			param = funcTy.Signature.Params.Params[i]
		}

		argTy := c.Synth(arg)

		switch paramTy := param.Type.(type) {
		case *tree.TypeParam:
			c.TyCtx.AddEq(argTy, paramTy)
		default:
			c.CheckAssignableTo(argTy, paramTy)
		}
	}
	returns := []tree.Type{}
	for _, result := range funcTy.Signature.Results.Params {
		returns = append(returns, result.Type)
	}
	switch len(returns) {
	case 0:
		return &tree.TupleType{Elems: []tree.Type{}}
	case 1:
		return returns[0]
	default:
		return &tree.TupleType{Elems: returns}
	}
}

func (c *Checker) SynthFuncLitExpr(expr *tree.FuncLitExpr) tree.Type {
	return &tree.FunctionType{Signature: expr.Signature}
}

func (c *Checker) SynthConversionExpr(expr *tree.ConversionExpr) tree.Type {
	// TODO check conversion
	return expr.Type
}

func (c *Checker) SynthBuiltinConversion(expr tree.Expr, targetTy *tree.TypeBuiltin) tree.Type {
	exprTy := c.Synth(expr)
	switch exprTy := exprTy.(type) {
	case *tree.UntypedConstantType:
		if exprTy.IsCompatible(targetTy.Name.Value) {
			return targetTy
		}
	case *tree.TypeBuiltin:
		if exprTy.Name.Value == "Pointer" {
			if targetTy.Name.Value == "uintptr" {
				return targetTy
			}
		}
	}
	panic(fmt.Sprintf("cannot convert %v to %v", exprTy, targetTy))
}

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
	case "panic":
		return c.SynthBuiltinPanicCall(expr)
	case "print":
		return c.SynthBuiltinPrintCall(expr)
	case "println":
		return c.SynthBuiltinPrintlnCall(expr)
	case "copy":
		return c.SynthBuiltinCopyCall(expr)
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

	if !c.HasLen(argTy) {
		panic(fmt.Sprintf("len() on incompatible type %v", argTy))
	}

	return c.BuiltinType("int")
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

	c.TyCtx.AddEq(elemTy1, elemTy2)

	return c.BuiltinType("int")
}

func (c *Checker) SynthLiteralExpr(expr *tree.LiteralExpr) tree.Type {
	// TODO untype literal types
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
	return c.MakeCompositeLit(expr, c.ResolveType(expr.Type))
}

func (c *Checker) MakeCompositeLit(expr *tree.CompositeLitExpr, targetTy tree.Type) tree.Type {
	underTy := c.Under(targetTy)
	switch underTy := underTy.(type) {
	case *tree.PointerType:
		elemTy := c.ResolveType(underTy.ElemType)
		if _, ok := c.Under(elemTy).(*tree.PointerType); ok {
			panic("composite literal of double pointer type?")
		}
		return &tree.PointerType{ElemType: c.MakeCompositeLit(expr, elemTy)}
	}

	switch exprTy := c.Under(targetTy).(type) {
	case *tree.StructType:
		return c.MakeCompositeLitStruct(expr, exprTy)
	case *tree.SliceType:
		return c.MakeCompositeLitSlice(expr, exprTy)
	case *tree.MapType:
		return c.MakeCompositeLitMap(expr, exprTy)
	case *tree.ArrayType:
		return c.MakeCompositeLitArray(expr, exprTy)
	default:
		spew.Dump(exprTy)
		panic("unreachable")
	}
}

func (c *Checker) MakeCompositeLitStruct(expr *tree.CompositeLitExpr, structTy *tree.StructType) tree.Type {
	if len(expr.Elems) == 0 {
		return expr.Type
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
			c.CheckExpr(elem.Value, field.Type)
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
					c.CheckExpr(elem.Value, field.Type)
					continue elems
				}
			}
			panic(fmt.Sprintf("type %v has no field %v", structTy, elem.Key))
		}
	}

	return expr.Type
}

func (c *Checker) MakeCompositeLitSlice(expr *tree.CompositeLitExpr, sliceTy *tree.SliceType) tree.Type {
	for _, elem := range expr.Elems {
		c.CheckExpr(elem.Value, c.ResolveType(sliceTy.ElemType))
	}
	return expr.Type
}

func (c *Checker) MakeCompositeLitMap(expr *tree.CompositeLitExpr, mapTy *tree.MapType) tree.Type {
	for _, elem := range expr.Elems {
		c.CheckExpr(elem.Key, c.ResolveType(mapTy.KeyType))
		c.CheckExpr(elem.Value, c.ResolveType(mapTy.ValueType))
	}
	return expr.Type
}

func (c *Checker) MakeCompositeLitArray(expr *tree.CompositeLitExpr, arrayTy *tree.ArrayType) tree.Type {
	var arrayLen int64
	switch arrayTy.Len.(type) {
	case *tree.EllipsisExpr:
		arrayLen = -1
	default:
		arrayLen = c.EvaluateConstantIntExpr(nil, arrayTy.Len)
		if int64(len(expr.Elems)) > arrayLen {
			panic("composite literal with wrong number of elements")
		}
	}
	for _, elem := range expr.Elems {
		c.CheckExpr(elem.Value, arrayTy.ElemType)
	}
	if arrayLen == -1 {
		return &tree.ArrayType{ElemType: arrayTy.ElemType, Len: &tree.ConstIntExpr{Value: int64(len(expr.Elems))}}
	} else {
		return &tree.ArrayType{ElemType: arrayTy.ElemType, Len: &tree.ConstIntExpr{Value: arrayLen}}
	}
}
