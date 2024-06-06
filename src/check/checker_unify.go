package check

import (
	"fmt"
	"github.com/davecgh/go-spew/spew"
	"github.com/garciat/gobid/tree"
)

const DebugUnify = false

func UnifyPrintf(format string, args ...interface{}) {
	if DebugUnify {
		fmt.Printf(format, args...)
	}
}

func (c *Checker) CheckSubst(tyParams *tree.TypeParamList, subst Subst) {
	for _, tyParam := range tyParams.Params {
		tySub, ok := subst[tyParam.Name]
		if !ok {
			continue // TODO: is this ok?
		}
		if single, ok := IsSingleTypeUnion(tyParam.Constraint); ok {
			if c.Identical(tySub, c.ResolveType(single)) {
				continue
			}
		}
		panic(fmt.Sprintf("type param %v with constraint %v cannot be %v", tyParam.Name, tyParam.Constraint, tySub))
	}
}

func (c *Checker) Verify() Subst {
	subst := Subst{}

	UnifyPrintf("=== Verify ===")

	for i := 0; i < 10; i++ {
		UnifyPrintf("=== iteration %d ===\n", i)
		UnifyPrintf("%v\n", c.TyCtx)

		learned := Subst{}

		c.Unify(c.TyCtx.Relations, learned)
		learned = c.Simplify(learned)

		UnifyPrintf("learned: %v\n", learned)

		next := []Relation{}

		for _, rel := range c.TyCtx.Relations {
			switch rel := rel.(type) {
			case RelationEq:
				next = append(next, RelationEq{
					Left:  c.ApplySubst(rel.Left, learned),
					Right: c.ApplySubst(rel.Right, learned),
				})
			case RelationSubtype:
				next = append(next, RelationSubtype{
					Sub:   c.ApplySubst(rel.Sub, learned),
					Super: c.ApplySubst(rel.Super, learned),
				})
			case RelationSatisfies:
				next = append(next, RelationSatisfies{
					Type:       c.ApplySubst(rel.Type, learned),
					Constraint: c.ApplySubst(rel.Constraint, learned).(*tree.InterfaceType),
				})
			default:
				panic("unreachable")
			}
		}

		c.TyCtx.Relations = next
		subst = c.Merge(subst, learned)

		if len(learned) == 0 {
			break
		}
	}

	subst = c.Simplify(subst)

	UnifyPrintf("=== subst ===")
	UnifyPrintf("%v\n", subst)

	return subst
}

func (c *Checker) Unify(rels []Relation, subst Subst) {
	for _, rel := range rels {
		switch rel := rel.(type) {
		case RelationEq:
			c.UnifyEq(rel.Left, rel.Right, subst)
		case RelationSubtype:
			c.UnifySubtype(rel.Sub, rel.Super, subst)
		case RelationSatisfies:
			c.UnifySatisfies(rel.Type, rel.Constraint, subst)
		default:
			panic("unreachable")
		}
	}
}

func IntersectInterfaces(elems ...tree.InterfaceType) *tree.InterfaceType {
	inter := &tree.InterfaceType{Methods: nil, Constraints: nil}
	for _, elem := range elems {
		inter.Methods = append(inter.Methods, elem.Methods...)
		inter.Constraints = append(inter.Constraints, elem.Constraints...)
	}
	return inter
}

func (c *Checker) UnifyEq(left, right tree.Type, subst Subst) {
	left = c.ResolveType(left)
	right = c.ResolveType(right)

	UnifyPrintf("? %v = %v %v\n", left, right, subst)

	if c.Identical(left, right) {
		return
	}

	if _, ok := right.(*tree.TypeParam); ok {
		left, right = right, left
	}

	if _, ok := left.(*tree.NilType); ok {
		left, right = right, left
	}

	if _, ok := right.(*tree.NilType); ok {
		switch left := c.Under(left).(type) {
		case *tree.PointerType:
			return
		case *tree.ChannelType:
			return
		case *tree.FunctionType:
			return
		case *tree.InterfaceType:
			return
		case *tree.MapType:
			return
		case *tree.SliceType:
			return
		case *tree.TypeBuiltin:
			if left.Name.Value == "Pointer" {
				return
			}
			panic(fmt.Sprintf("cannot assign nil to type %v", left))
		default:
			panic(fmt.Sprintf("cannot assign nil to type %v", left))
		}
	}

	switch left := left.(type) {
	case *tree.TypeBuiltin:
		if right, ok := right.(*tree.TypeBuiltin); ok {
			if c.IsNumeric(left) && c.IsNumeric(right) {
				return // TODO ok?
			}
			panic(fmt.Sprintf("cannot unify: %v = %v", left, right))
		}
		c.UnifyEq(right, left, subst)
	case *tree.TypeParam:
		if right, ok := right.(*tree.UntypedConstantType); ok {
			c.UnifyEq(left, c.UntypedDefaultType(right), subst)
			return
		}
		if s, ok := subst[left.Name]; ok {
			if !c.Identical(s, right) {
				c.UnifyEq(s, right, subst)
			}
		} else {
			subst[left.Name] = right
		}
	case *tree.SliceType:
		if right, ok := right.(*tree.SliceType); ok {
			c.UnifyEq(left.ElemType, right.ElemType, subst)
			return
		}
		panic(fmt.Sprintf("cannot unify: %v = %v", left, right))
	case *tree.ArrayType:
		if right, ok := right.(*tree.ArrayType); ok {
			leftLen := c.EvaluateConstantIntExpr(nil, left.Len)
			rightLen := c.EvaluateConstantIntExpr(nil, right.Len)
			if leftLen != rightLen {
				panic(fmt.Sprintf("cannot unify: %v = %v (different array length)", left, right))
			}
			c.UnifyEq(left.ElemType, right.ElemType, subst)
			return
		}
	case *tree.InterfaceType:
		left = c.SimplifyInterface(left)
		if single, ok := IsSingleTypeUnion(left); ok {
			c.UnifyEq(single, right, subst)
			return
		}
		if _, ok := right.(*tree.NilType); ok {
			return
		}
		spew.Dump(left, right)
		panic("TODO")
	case *tree.TypeApplication:
		if right, ok := right.(*tree.TypeApplication); ok {
			if left.Type != right.Type {
				panic(fmt.Sprintf("cannot unify: %v = %v", left, right))
			}
			if len(left.Args) != len(right.Args) {
				panic(fmt.Sprintf("cannot unify: %v = %v", left, right))
			}
			for i, leftArg := range left.Args {
				c.UnifyEq(leftArg, right.Args[i], subst)
			}
			return
		}
		panic(fmt.Sprintf("cannot unify: %v = %v", left, right))
	case *tree.PointerType:
		if right, ok := right.(*tree.PointerType); ok {
			c.UnifyEq(left.ElemType, right.ElemType, subst)
			return
		}
		panic(fmt.Sprintf("cannot unify: %v = %v", left, right))
	case *tree.NamedType:
		c.UnifyEq(c.Under(left), c.Under(right), subst)
	case *tree.UntypedConstantType:
		if right, ok := right.(*tree.TypeBuiltin); ok {
			if left.IsCompatible(right.Name.Value) {
				return
			}
		}
		c.UnifyEq(c.UntypedDefaultType(left), right, subst)
	case *tree.FunctionType:
		if right, ok := right.(*tree.FunctionType); ok {
			if len(left.Signature.TypeParams.Params) != 0 {
				panic("cannot unify generic functions")
			}
			if len(right.Signature.TypeParams.Params) != 0 {
				panic("cannot unify generic functions")
			}
			if len(left.Signature.Params.Params) != len(right.Signature.Params.Params) {
				panic("cannot unify functions with different number of parameters")
			}
			if len(left.Signature.Results.Params) != len(right.Signature.Results.Params) {
				panic("cannot unify functions with different number of results")
			}
			for i := range left.Signature.Params.Params {
				c.UnifyEq(left.Signature.Params.Params[i].Type, right.Signature.Params.Params[i].Type, subst)
			}
			for i := range left.Signature.Results.Params {
				c.UnifyEq(left.Signature.Results.Params[i].Type, right.Signature.Results.Params[i].Type, subst)
			}
			return // OK
		}
		panic(fmt.Sprintf("cannot unify: %v = %v", left, right))
	case *tree.StructType:
		if right, ok := c.Under(right).(*tree.StructType); ok {
			if c.Identical(left, right) {
				return
			}
		}
		panic(fmt.Sprintf("cannot unify: %v = %v", left, right))
	default:
		spew.Dump(left, right)
		panic("unreachable")
	}
}

func (c *Checker) UnifySubtype(sub, super tree.Type, subst Subst) {
	sub = c.ResolveType(sub)
	super = c.ResolveType(super)

	UnifyPrintf("? %v <: %v %v\n", sub, super, subst)

	if sub, ok := c.ResolveType(sub).(*tree.TypeParam); ok {
		if !c.Identical(sub, super) && c.ContainsTypeParam(super, sub) {
			panic(fmt.Sprintf("circular constraint: %v <: %v", sub, super))
		}
	}
	if super, ok := c.ResolveType(super).(*tree.TypeParam); ok {
		if !c.Identical(sub, super) && c.ContainsTypeParam(sub, super) {
			panic(fmt.Sprintf("circular constraint: %v <: %v", sub, super))
		}
	}

	if _, ok := sub.(*tree.NilType); ok {
		switch super := c.Under(super).(type) {
		case *tree.PointerType:
			return
		case *tree.ChannelType:
			return
		case *tree.FunctionType:
			return
		case *tree.InterfaceType:
			return
		case *tree.MapType:
			return
		case *tree.SliceType:
			return
		case *tree.TypeBuiltin:
			if super.Name.Value == "Pointer" {
				return
			}
			panic(fmt.Sprintf("cannot assign nil to type %v", super))
		default:
			panic(fmt.Sprintf("cannot assign nil to type %v", super))
		}
	}

	if c.IsConcreteType(super) {
		c.UnifyEq(sub, super, subst)
		return
	}

	switch super := super.(type) {
	case *tree.InterfaceType:
		var typeset *TypeSet
		c.BasicSatisfy(sub, super, subst, &typeset)
		if typeset != nil && !typeset.Universe {
			// TODO hacky???
			panic(fmt.Sprintf("cannot assign %v to %v", sub, super))
		}
	case *tree.TypeApplication:
		c.UnifySubtype(sub, c.Under(super), subst) // TODO: adding more constraints?
	case *tree.NamedType:
		if subTy, ok := sub.(*tree.NamedType); ok {
			if subTy.Name == super.Name {
				return
			}
		}
		c.UnifySubtype(sub, c.Under(super), subst)
	case *tree.TypeParam:
		if subTy, ok := c.ResolveType(sub).(*tree.TypeParam); ok {
			if subTy.Name == super.Name {
				return
			}
		}
		if super.Bound != nil {
			c.UnifySubtype(sub, super.Bound, subst)
		}
	default:
		spew.Dump(sub, super)
		panic("unreachable")
	}
}

func (c *Checker) UnifySatisfies(sub tree.Type, inter *tree.InterfaceType, subst Subst) {
	sub = c.ResolveType(sub)

	var typeset *TypeSet
	c.BasicSatisfy(sub, inter, subst, &typeset)

	if typeset != nil && !typeset.Universe {
		for _, term := range typeset.Types {
			termTy := c.ResolveType(term)
			if !c.IsConcreteType(termTy) {
				panic("cannot make union of non-concrete types")
			}
			if c.Identical(sub, termTy) {
				c.UnifyEq(sub, termTy, subst) // necessary?
				return
			}
		}
		panic(fmt.Sprintf("type %v does not satisfy %v", sub, inter))
	}
}

// TODO this seems unprincipled
func (c *Checker) BasicSatisfy(sub tree.Type, inter *tree.InterfaceType, subst Subst, out **TypeSet) {
	inter = c.SimplifyInterface(inter)
	supertypeset := c.InterfaceTypeSet(inter)
	if !supertypeset.Universe && len(supertypeset.Types) == 0 {
		panic("cannot satisfy empty set")
	}
	if len(supertypeset.Types) == 1 {
		single := c.ResolveType(supertypeset.Types[0])
		// TODO hacky
		if c.Identical(sub, single) {
			return
		}
		if c.IsConcreteType(single) {
			c.UnifyEq(sub, single, subst)
		}
		// c.UnifySatisfies(sub, &InterfaceType{Methods: supertypeset.Methods}, subst)
		c.CheckAssignableTo(sub, &tree.InterfaceType{Methods: supertypeset.Methods})
		return // leave for next iteration?
	}
	if len(supertypeset.Methods) > 0 {
		if err := c.CheckMethodsSatisfy(sub, supertypeset.Methods); err != nil {
			panic(fmt.Errorf("type %v does not satisfy %v: %v", sub, inter, err))
		}
	}
	if tyPar, ok := c.ResolveType(sub).(*tree.TypeParam); ok {
		var bound tree.Type = tyPar.Bound
		if tyPar.Bound == nil {
			bound = tree.EmptyInterface()
		}
		c.UnifySubtype(bound, inter, subst)
		return
	}
	if sub, ok := c.ResolveType(sub).(*tree.InterfaceType); ok {
		subtypeset := c.InterfaceTypeSet(sub)
		if len(subtypeset.Methods) != 0 {
			panic("TODO")
		}
		for _, term := range subtypeset.Types {
			termTy := c.ResolveType(term)
			found := false
			for _, superTerm := range supertypeset.Types {
				superTermTy := c.ResolveType(superTerm)
				if c.Identical(termTy, superTermTy) {
					found = true
				}
			}
			if !found {
				panic(fmt.Sprintf("interface %v does not satisfy %v", sub, inter))
			}
		}
		return
	}
	if len(supertypeset.Types) == 1 {
		c.UnifySubtype(sub, supertypeset.Types[0], subst)
		return
	}
	*out = &supertypeset
}
