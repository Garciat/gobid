package check

import (
	"fmt"
	"github.com/davecgh/go-spew/spew"
	"github.com/garciat/gobid/tree"
)

func (c *Checker) Verify() Subst {
	subst := Subst{}

	UnifyPrintf("=== Verify ===\n")

	for i := 0; i < 10; i++ {
		UnifyPrintf("=== iteration %d ===\n", i)
		UnifyPrintf("Context:\n%v\n", c.TyCtx)

		learned := Subst{}

		UnifyPrintf("Steps:\n")
		c.Unify(c.TyCtx.Relations, learned)
		learned = c.Simplify(learned)

		// TODO weird to mutate this
		c.TyCtx.Relations = c.ApplySubstRelations(c.TyCtx.Relations, learned)

		subst = c.Merge(subst, learned)

		if len(learned) == 0 {
			break
		}
	}

	subst = c.Simplify(subst)

	UnifyPrintf("=== DONE ===\n")
	UnifyPrintf("%v\n", subst)

	for _, ty := range c.GetCurrentScopeTypeInstantiations() {
		if _, ok := subst[ty.Name]; !ok {
			panic(fmt.Errorf("type instantiation %v not resolved", ty))
		}
	}

	return subst
}

func (c *Checker) Unify(rels []Relation, subst Subst) {
	// TODO consider popping relations off the stack; re-adding them if they are not resolved
	for _, rel := range rels {
		switch rel := rel.(type) {
		case RelationEq:
			if err := c.UnifyEq(rel.Left, rel.Right, subst); err != nil {
				panic(err)
			}
		case RelationSubtype:
			if err := c.UnifySubtype(rel.Sub, rel.Super, subst); err != nil {
				panic(err)
			}
		case RelationSatisfies:
			if err := c.UnifySatisfies(rel.Type, rel.Constraint, subst); err != nil {
				panic(err)
			}
		default:
			panic("unreachable")
		}
	}
}

func (c *Checker) UnifyEq(left, right tree.Type, subst Subst) error {
	left = c.ResolveType(left)
	right = c.ResolveType(right)

	if _, ok := right.(*tree.TypeParam); ok {
		left, right = right, left
	}

	if _, ok := left.(*tree.NilType); ok {
		left, right = right, left
	}

	UnifyPrintf("? %v = %v %v\n", left, right, subst)

	if c.Identical(left, right) {
		return nil
	}

	if _, ok := right.(*tree.NilType); ok {
		switch left := c.Under(left).(type) {
		case *tree.TypeParam:
			return nil // TODO for future iterations?
		case *tree.PointerType:
			return nil
		case *tree.ChannelType:
			return nil
		case *tree.FunctionType:
			return nil
		case *tree.InterfaceType:
			return nil
		case *tree.MapType:
			return nil
		case *tree.SliceType:
			return nil
		case *tree.BuiltinType:
			if left.Tag == tree.BuiltinTypeTagUnsafePointer {
				return nil
			}
			return fmt.Errorf("cannot assign nil to type %v", left)
		default:
			return fmt.Errorf("cannot assign nil to type %v", left)
		}
	}

	switch left := left.(type) {
	case *tree.BuiltinType:
		switch right := right.(type) {
		case *tree.BuiltinType:
			if left.Tag == right.Tag {
				return nil
			}
		case *tree.UntypedConstantType:
			if right.IsAssignableTo(left) {
				return nil
			}
		}
	case *tree.TypeParam:
		if right, ok := right.(*tree.UntypedConstantType); ok {
			if err := c.UnifyEq(left, right.DefaultType(), subst); err != nil {
				return fmt.Errorf("when unifying %v = %v:\n%w", left, right, err)
			}
			return nil
		}
		if s, ok := subst[left.Name]; ok {
			if !c.Identical(s, right) {
				if err := c.UnifyEq(s, right, subst); err != nil {
					return fmt.Errorf("when unifying %v = %v:\n%w", s, right, err)
				}
			}
			return nil
		} else {
			UnifyPrintf("learned: %v -> %v\n", left, right)
			subst[left.Name] = right
			return nil
		}
	case *tree.SliceType:
		if right, ok := right.(*tree.SliceType); ok {
			if err := c.UnifyEq(left.ElemType, right.ElemType, subst); err != nil {
				return fmt.Errorf("when unifying %v = %v:\n%w", left, right, err)
			}
			return nil
		}
	case *tree.ArrayType:
		if right, ok := right.(*tree.ArrayType); ok {
			leftLen := c.EvaluateConstantIntExpr(nil, left.Len)
			rightLen := c.EvaluateConstantIntExpr(nil, right.Len)
			if leftLen != rightLen {
				panic(fmt.Errorf("cannot unify: %v = %v (different array length)", left, right))
			}
			if err := c.UnifyEq(left.ElemType, right.ElemType, subst); err != nil {
				return fmt.Errorf("when unifying %v = %v:\n%w", left, right, err)
			}
			return nil
		}
	case *tree.InterfaceType:
		left = c.SimplifyInterface(left)
		if single, ok := IsSingleTypeUnion(left); ok {
			if err := c.UnifyEq(single, right, subst); err != nil {
				return fmt.Errorf("when unifying %v = %v:\n%w", left, right, err)
			}
			return nil
		}
		if _, ok := right.(*tree.NilType); ok {
			return nil
		}
	case *tree.TypeApplication:
		if right, ok := right.(*tree.TypeApplication); ok {
			if !c.Identical(c.ResolveType(left.Type), c.ResolveType(right.Type)) {
				return fmt.Errorf("cannot unify: %v = %v", left, right)
			}
			if len(left.Args) != len(right.Args) {
				return fmt.Errorf("cannot unify: %v = %v", left, right)
			}
			for i, leftArg := range left.Args {
				if err := c.UnifyEq(leftArg, right.Args[i], subst); err != nil {
					return fmt.Errorf("when unifying %v = %v:\n%w", left, right, err)
				}
			}
			return nil
		}
	case *tree.PointerType:
		if right, ok := right.(*tree.PointerType); ok {
			if err := c.UnifyEq(c.ResolveType(left.ElemType), c.ResolveType(right.ElemType), subst); err != nil {
				return fmt.Errorf("when unifying %v = %v:\n%w", left, right, err)
			}
			return nil
		}
	case *tree.NamedType:
		if err := c.UnifyEq(c.Under(left), right, subst); err != nil {
			return fmt.Errorf("when unifying %v = %v:\n%w", left, right, err)
		}
	case *tree.UntypedConstantType:
		if right, ok := right.(*tree.BuiltinType); ok {
			if left.IsAssignableTo(right) {
				return nil
			}
		}
		if err := c.UnifyEq(left.DefaultType(), right, subst); err != nil {
			return fmt.Errorf("when unifying %v = %v:\n%w", left, right, err)
		}
		return nil
	case *tree.FunctionType:
		if right, ok := right.(*tree.FunctionType); ok {
			if len(left.Signature.TypeParams.Params) != 0 {
				return fmt.Errorf("cannot unify generic functions")
			}
			if len(right.Signature.TypeParams.Params) != 0 {
				return fmt.Errorf("cannot unify generic functions")
			}
			if len(left.Signature.Params.Params) != len(right.Signature.Params.Params) {
				return fmt.Errorf("cannot unify functions with different number of parameters")
			}
			if len(left.Signature.Results.Params) != len(right.Signature.Results.Params) {
				return fmt.Errorf("cannot unify functions with different number of results")
			}
			for i := range left.Signature.Params.Params {
				err := c.UnifyEq(left.Signature.Params.Params[i].Type, right.Signature.Params.Params[i].Type, subst)
				if err != nil {
					return fmt.Errorf("when unifying %v = %v:\n%w", left, right, err)
				}
			}
			for i := range left.Signature.Results.Params {
				err := c.UnifyEq(left.Signature.Results.Params[i].Type, right.Signature.Results.Params[i].Type, subst)
				if err != nil {
					return fmt.Errorf("when unifying %v = %v:\n%w", left, right, err)
				}
			}
			return nil // OK
		}
	case *tree.StructType:
		if right, ok := c.Under(right).(*tree.StructType); ok {
			if c.Identical(left, right) {
				return nil
			}
		}
	case *tree.ChannelType:
		if right, ok := right.(*tree.ChannelType); ok {
			if left.Dir != right.Dir {
				panic(fmt.Errorf("cannot unify: %v = %v", left, right))
			}
			err := c.UnifyEq(c.ResolveValue(left.ElemType), c.ResolveType(right.ElemType), subst)
			if err != nil {
				return fmt.Errorf("when unifying %v = %v:\n%w", left, right, err)
			}
			return nil
		}
	default:
		spew.Dump(left, right)
		panic("unreachable")
	}

	return fmt.Errorf("cannot unify: %v = %v", left, right)
}

func (c *Checker) UnifySubtype(sub, super tree.Type, subst Subst) error {
	sub = c.ResolveType(sub)
	super = c.ResolveType(super)

	UnifyPrintf("? %v <: %v %v\n", sub, super, subst)

	if sub, ok := c.ResolveType(sub).(*tree.TypeParam); ok {
		if !c.Identical(sub, super) && c.ContainsTypeParam(super, sub) {
			return fmt.Errorf("circular constraint: %v <: %v", sub, super)
		}
	}
	if super, ok := c.ResolveType(super).(*tree.TypeParam); ok {
		if !c.Identical(sub, super) && c.ContainsTypeParam(sub, super) {
			return fmt.Errorf("circular constraint: %v <: %v", sub, super)
		}
	}

	if _, ok := sub.(*tree.NilType); ok {
		switch super := c.Under(super).(type) {
		case *tree.PointerType:
			return nil
		case *tree.ChannelType:
			return nil
		case *tree.FunctionType:
			return nil
		case *tree.InterfaceType:
			return nil
		case *tree.MapType:
			return nil
		case *tree.SliceType:
			return nil
		case *tree.BuiltinType:
			if super.Tag == tree.BuiltinTypeTagUnsafePointer {
				return nil
			}
			return fmt.Errorf("cannot assign nil to type %v", super)
		default:
			return fmt.Errorf("cannot assign nil to type %v", super)
		}
	}

	if c.IsConcreteType(super) {
		if err := c.UnifyEq(sub, super, subst); err != nil {
			return fmt.Errorf("when unifying %v <: %v:\n%w", sub, super, err)
		}
		return nil
	}

	switch super := super.(type) {
	case *tree.InterfaceType:
		var typeset *TypeSet
		err := c.BasicSatisfy(sub, super, subst, &typeset)
		if err != nil {
			return fmt.Errorf("when unifying %v <: %v:\n%w", sub, super, err)
		}
		if !(typeset != nil && !typeset.Universe) {
			// TODO hacky???
			return nil
		}
	case *tree.TypeApplication:
		if err := c.UnifySubtype(sub, c.TypeApplication(super), subst); err != nil {
			return fmt.Errorf("when unifying %v <: %v:\n%w", sub, super, err)
		}
		return nil
	case *tree.NamedType:
		if subTy, ok := sub.(*tree.NamedType); ok {
			if subTy.SameType(super) {
				return nil
			}
		}
		if err := c.UnifySubtype(sub, c.Under(super), subst); err != nil {
			return fmt.Errorf("when unifying %v <: %v:\n%w", sub, super, err)
		}
		return nil
	case *tree.TypeParam:
		if subTy, ok := c.ResolveType(sub).(*tree.TypeParam); ok {
			if subTy.Name == super.Name {
				return nil
			}
			if subTy.Bound != nil {
				if subTy, ok := IsSingleTypeUnion(subTy.Bound); ok {
					if err := c.UnifySubtype(subTy, super, subst); err != nil {
						return fmt.Errorf("when unifying %v <: %v:\n%w", sub, super, err)
					}
					return nil
				}
			}
		}
		if super.Bound != nil {
			if err := c.UnifySubtype(sub, super.Bound, subst); err != nil {
				return fmt.Errorf("when unifying %v <: %v:\n%w", sub, super, err)
			}
			return nil
		}
	default:
		spew.Dump(sub, super)
		panic("unreachable")
	}

	return fmt.Errorf("cannot unify %v <: %v", sub, super)
}

func (c *Checker) UnifySatisfies(sub tree.Type, inter *tree.InterfaceType, subst Subst) error {
	sub = c.ResolveType(sub)

	switch sub.(type) {
	case *tree.TypeParam:
		return nil // TODO for future iterations?
	}

	var typeset *TypeSet
	err := c.BasicSatisfy(sub, inter, subst, &typeset)
	if err != nil {
		return fmt.Errorf("when unifying %v satisfies %v:\n%w", sub, inter, err)
	}

	if typeset != nil && !typeset.Universe {
		for _, term := range typeset.Terms {
			termTy := c.ResolveType(term.Type)
			if !c.IsConcreteType(termTy) {
				// TODO check this earlier?
				return fmt.Errorf("cannot make union of non-concrete types")
			}
			if term.Tilde {
				sub = c.Under(sub)
			}
			if c.Identical(sub, termTy) {
				return nil
			}
		}
		return fmt.Errorf("type %v does not satisfy %v", sub, inter)
	}

	return nil
}

// TODO this seems unprincipled

func (c *Checker) BasicSatisfy(sub tree.Type, inter *tree.InterfaceType, subst Subst, out **TypeSet) error {
	inter = c.SimplifyInterface(inter)
	supertypeset := c.InterfaceTypeSet(inter)
	if !supertypeset.Universe && len(supertypeset.Terms) == 0 {
		return fmt.Errorf("cannot satisfy empty set")
	}
	if len(supertypeset.Terms) == 1 {
		term := supertypeset.Terms[0]
		single := c.ResolveType(term.Type)
		if term.Tilde {
			sub = c.Under(sub)
		}
		// TODO hacky
		if c.Identical(sub, single) {
			return nil
		}
		if c.IsConcreteType(single) {
			if err := c.UnifyEq(sub, single, subst); err != nil {
				return err
			}
		}
		// c.UnifySatisfies(sub, &InterfaceType{Methods: supertypeset.Methods}, subst)
		c.CheckAssignableTo(sub, &tree.InterfaceType{Methods: supertypeset.Methods})
		return nil // leave for next iteration?
	}
	if len(supertypeset.Methods) > 0 {
		if err := c.CheckMethodsSatisfy(sub, supertypeset.Methods); err != nil {
			return err
		}
	}
	if tyPar, ok := c.ResolveType(sub).(*tree.TypeParam); ok {
		var bound tree.Type = tyPar.Bound
		if tyPar.Bound == nil {
			bound = tree.EmptyInterface()
		}
		return c.UnifySubtype(bound, inter, subst)
	}
	if sub, ok := c.ResolveType(sub).(*tree.InterfaceType); ok {
		subtypeset := c.InterfaceTypeSet(sub)
		for _, superMethod := range supertypeset.Methods {
			found := false
			for _, subMethod := range subtypeset.Methods {
				if superMethod.Name == subMethod.Name {
					if !c.Identical(superMethod.Type, subMethod.Type) {
						panic(fmt.Errorf("interface %v does not satisfy %v", sub, inter))
					}
					found = true
				}
			}
			if !found {
				panic(fmt.Errorf("interface %v does not satisfy %v", sub, inter))
			}
		}
		for _, term := range subtypeset.Terms {
			termTy := c.ResolveType(term.Type)
			found := false
			for _, superTerm := range supertypeset.Terms {
				superTermTy := c.ResolveType(superTerm.Type)
				if term.Tilde == superTerm.Tilde && c.Identical(termTy, superTermTy) {
					found = true
				}
			}
			if !found {
				return fmt.Errorf("interface %v does not satisfy %v", sub, inter)
			}
		}
		return nil
	}
	if len(supertypeset.Terms) == 1 {
		return c.UnifySubtype(sub, supertypeset.Terms[0].Type, subst)
	}
	*out = &supertypeset
	return nil
}
