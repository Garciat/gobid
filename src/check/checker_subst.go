package check

import (
	"fmt"
	"github.com/davecgh/go-spew/spew"
	"github.com/garciat/gobid/common"
	"github.com/garciat/gobid/tree"
	"strings"
)

type Subst map[common.Identifier]tree.Type

func (s Subst) String() string {
	parts := make([]string, 0, len(s))
	for k, v := range s {
		parts = append(parts, fmt.Sprintf("%v -> %v", k, v))
	}
	return fmt.Sprintf("{{ %v }}", strings.Join(parts, " ; "))
}

func (c *Checker) Simplify(subst Subst) Subst {
	next := Subst{}
	for k, v := range subst {
		next[k] = c.ApplySubst(v, subst)
	}
	return next
}

func (c *Checker) Merge(lhs, rhs Subst) Subst {
	result := Subst{}
	for k, v := range lhs {
		result[k] = v
	}
	for k, v := range rhs {
		if _, ok := result[k]; ok {
			if !c.Identical(result[k], v) {
				spew.Dump(k, v)
				panic("incompatible substitutions")
			}
		}
		result[k] = v
	}
	return result
}

func (c *Checker) ApplySubst(ty tree.Type, subst Subst) tree.Type {
	switch ty := ty.(type) {
	case *tree.TypeParam:
		if substTy, ok := subst[ty.Name]; ok {
			return substTy
		}
		return ty
	case *tree.TypeName:
		// TODO kinda sus
		if substTy, ok := subst[ty.Name]; ok {
			return substTy
		}
		return ty
	case *tree.ImportTypeName:
		return ty
	case *tree.PackageTypeName:
		return ty
	case *tree.TypeBuiltin:
		return ty
	case *tree.TypeApplication:
		args := make([]tree.Type, len(ty.Args))
		for i, arg := range ty.Args {
			args[i] = c.ApplySubst(arg, subst)
		}
		return &tree.TypeApplication{Type: ty.Type, Args: args}
	case *tree.ArrayType:
		return &tree.ArrayType{
			ElemType: c.ApplySubst(ty.ElemType, subst),
			Len:      ty.Len,
		}
	case *tree.FunctionType:
		return &tree.FunctionType{
			Signature: c.ApplySubstSignature(ty.Signature, subst),
		}
	case *tree.GenericType:
		return &tree.GenericType{
			TypeParams: c.ApplySubstTypeParamList(ty.TypeParams, subst),
			Type:       c.ApplySubst(ty.Type, subst),
		}
	case *tree.StructType:
		fields := make([]*tree.FieldDecl, len(ty.Fields))
		for i, field := range ty.Fields {
			fields[i] = &tree.FieldDecl{
				Name: field.Name,
				Type: c.ApplySubst(field.Type, subst),
			}
		}
		return &tree.StructType{Fields: fields}
	case *tree.PointerType:
		return &tree.PointerType{ElemType: c.ApplySubst(ty.ElemType, subst)}
	case *tree.InterfaceType:
		constraints := make([]*tree.TypeConstraint, len(ty.Constraints))
		for i, constraint := range ty.Constraints {
			constraints[i] = &tree.TypeConstraint{TypeElem: c.ApplySubstTypeElem(constraint.TypeElem, subst)}
		}
		return &tree.InterfaceType{
			Methods:     c.ApplySubstMethodList(ty.Methods, subst),
			Constraints: constraints,
		}
	case *tree.SliceType:
		return &tree.SliceType{ElemType: c.ApplySubst(ty.ElemType, subst)}
	case *tree.MapType:
		return &tree.MapType{
			KeyType:   c.ApplySubst(ty.KeyType, subst),
			ValueType: c.ApplySubst(ty.ValueType, subst),
		}
	case *tree.ChannelType:
		return &tree.ChannelType{
			ElemType: c.ApplySubst(ty.ElemType, subst),
			Dir:      ty.Dir,
		}
	case *tree.TupleType:
		elems := make([]tree.Type, len(ty.Elems))
		for i, elem := range ty.Elems {
			elems[i] = c.ApplySubst(elem, subst)
		}
		return &tree.TupleType{Elems: elems}
	case *tree.NamedType:
		return &tree.NamedType{
			Name: ty.Name,
			Type: c.ApplySubst(ty.Type, subst),
			// Methods: c.ApplySubstMethodList(ty.Methods, subst),
		}
	case *tree.NilType:
		return ty
	case *tree.UntypedConstantType:
		return ty
	case *tree.TypeOfType:
		return &tree.TypeOfType{Type: c.ApplySubst(ty.Type, subst)}

	default:
		spew.Dump(ty)
		panic("unreachable")
	}
}

func (c *Checker) ApplySubstMethodList(methods []*tree.MethodElem, subst Subst) []*tree.MethodElem {
	out := make([]*tree.MethodElem, len(methods))
	for i, method := range methods {
		out[i] = &tree.MethodElem{
			Name: method.Name,
			Type: c.ApplySubst(method.Type, subst).(*tree.FunctionType),
		}
	}
	return out
}

func (c *Checker) ApplySubstSignature(sig *tree.Signature, subst Subst) *tree.Signature {
	return &tree.Signature{
		TypeParams: c.ApplySubstTypeParamList(sig.TypeParams, subst),
		Params:     c.ApplySubstParameterList(sig.Params, subst),
		Results:    c.ApplySubstParameterList(sig.Results, subst),
	}
}

func (c *Checker) ApplySubstTypeParamList(list *tree.TypeParamList, subst Subst) *tree.TypeParamList {
	params := make([]*tree.TypeParamDecl, len(list.Params))
	for i, param := range list.Params {
		var name common.Identifier
		if substTy, ok := subst[param.Name]; ok {
			name = substTy.(*tree.TypeParam).Name
		} else {
			name = param.Name
		}
		params[i] = &tree.TypeParamDecl{
			Name:       name,
			Constraint: c.ApplySubst(param.Constraint, subst).(*tree.InterfaceType),
		}
	}
	return &tree.TypeParamList{Params: params}
}

func (c *Checker) ApplySubstTypeElem(elem *tree.TypeElem, subst Subst) *tree.TypeElem {
	union := make([]*tree.TypeTerm, len(elem.Union))
	for i, term := range elem.Union {
		union[i] = &tree.TypeTerm{Type: c.ApplySubst(term.Type, subst), Tilde: term.Tilde}
	}
	return &tree.TypeElem{Union: union}
}

func (c *Checker) ApplySubstParameterList(list *tree.ParameterList, subst Subst) *tree.ParameterList {
	params := make([]*tree.ParameterDecl, len(list.Params))
	for i, param := range list.Params {
		params[i] = &tree.ParameterDecl{
			Name:     param.Name,
			Type:     c.ApplySubst(param.Type, subst),
			Variadic: param.Variadic,
		}
	}
	return &tree.ParameterList{Params: params}
}
