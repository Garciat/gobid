package check

import "github.com/garciat/gobid/tree"

func (c *Checker) TypeApplication(app *tree.TypeApplication) tree.Type {
	return c.TypeApplicationFunc(app, func(*tree.TypeParamDecl, tree.Type) {})
}

func (c *Checker) TypeApplicationFunc(app *tree.TypeApplication, argF func(tyParam *tree.TypeParamDecl, tyArg tree.Type)) tree.Type {
	named, subst := c.InstantiateTypeFunc(app, argF)
	gen := c.ResolveType(named.Type).(*tree.GenericType)
	return c.ApplySubst(gen.Type, subst)
}

func (c *Checker) InstantiateType(app *tree.TypeApplication) (*tree.NamedType, Subst) {
	return c.InstantiateTypeFunc(app, func(*tree.TypeParamDecl, tree.Type) {})
}

func (c *Checker) InstantiateTypeFunc(app *tree.TypeApplication, argF func(tyParam *tree.TypeParamDecl, tyArg tree.Type)) (*tree.NamedType, Subst) {
	named, ok := c.ResolveType(app.Type).(*tree.NamedType)
	if !ok {
		panic("can only instantiate named types?")
	}
	gen, ok := c.ResolveType(named.Type).(*tree.GenericType)
	if !ok {
		panic("not a generic type")
	}
	if len(gen.TypeParams.Params) != len(app.Args) {
		panic("wrong number of type arguments")
	}
	subst := Subst{}
	for i, tyArg := range app.Args {
		tyParam := gen.TypeParams.Params[i]
		subst[tyParam.Name] = tyArg
		c.TyCtx.AddRelation(RelationSatisfies{Type: tyArg, Constraint: tyParam.Constraint})
		argF(tyParam, tyArg)
	}
	return named, subst
}

func (c *Checker) ContainsTypeParam(ty tree.Type, tyParam *tree.TypeParam) bool {
	switch ty := ty.(type) {
	case *tree.TypeParam:
		return ty.Name == tyParam.Name
	case *tree.TypeApplication:
		for _, arg := range ty.Args {
			if c.ContainsTypeParam(arg, tyParam) {
				return true
			}
		}
		return false
	case *tree.PointerType:
		return c.ContainsTypeParam(ty.ElemType, tyParam)
	case *tree.StructType:
		for _, field := range ty.Fields {
			if c.ContainsTypeParam(field.Type, tyParam) {
				return true
			}
		}
		return false
	case *tree.InterfaceType:
		for _, constraint := range ty.Constraints {
			for _, term := range constraint.TypeElem.Union {
				if c.ContainsTypeParam(term.Type, tyParam) {
					return true
				}
			}
		}
		for _, method := range ty.Methods {
			if c.ContainsTypeParam(method.Type, tyParam) {
				return true
			}
		}
		return false
	case *tree.SliceType:
		return c.ContainsTypeParam(ty.ElemType, tyParam)
	case *tree.TypeBuiltin:
		return false
	case *tree.ArrayType:
		return c.ContainsTypeParam(ty.ElemType, tyParam)
	case *tree.FunctionType:
		// could check signature type params, but no nested type params?
		for _, param := range ty.Signature.Params.Params {
			if c.ContainsTypeParam(param.Type, tyParam) {
				return true
			}
		}
		for _, result := range ty.Signature.Results.Params {
			if c.ContainsTypeParam(result.Type, tyParam) {
				return true
			}
		}
		return false
	case *tree.GenericType:
		// cannot nest generic types?
		return false
	case *tree.TupleType:
		for _, elem := range ty.Elems {
			if c.ContainsTypeParam(elem, tyParam) {
				return true
			}
		}
		return false
	default:
		return false
	}
}
