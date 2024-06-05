package check

import (
	"github.com/davecgh/go-spew/spew"
	"github.com/garciat/gobid/tree"
)

type TypeSet struct {
	Methods  []*tree.MethodElem
	Types    []tree.Type
	Universe bool
}

func (c *Checker) Combine(lhs, rhs TypeSet) TypeSet {
	result := TypeSet{
		Methods:  []*tree.MethodElem{},
		Types:    []tree.Type{},
		Universe: lhs.Universe && rhs.Universe,
	}

	// combine
	copy(result.Methods, lhs.Methods)

	for _, m := range rhs.Methods {
		for _, n := range lhs.Methods {
			if m.Name == n.Name {
				if !c.Identical(m.Type, n.Type) {
					panic("method clash")
				}
				continue
			}
		}
		result.Methods = append(result.Methods, m)
	}

	if lhs.Universe && rhs.Universe {
		if len(lhs.Types) != 0 {
			panic("weird")
		}
		if len(rhs.Types) != 0 {
			panic("weird")
		}
	} else if lhs.Universe && !rhs.Universe {
		result.Types = append(result.Types, rhs.Types...)
	} else if !lhs.Universe && rhs.Universe {
		result.Types = append(result.Types, lhs.Types...)
	} else {
		// intersect
		for _, t := range rhs.Types {
			for _, u := range lhs.Types {
				if c.Identical(t, u) {
					result.Types = append(result.Types, t)
				}
			}
		}
	}

	return result
}

func (c *Checker) InterfaceTypeSet(ty *tree.InterfaceType) TypeSet {
	typeset := TypeSet{
		Methods:  ty.Methods,
		Universe: true,
	}

	if len(ty.Constraints) == 0 {
		return typeset
	}

	for _, constraint := range ty.Constraints {
		var next TypeSet
		if len(constraint.TypeElem.Union) == 1 {
			term := constraint.TypeElem.Union[0]
			if term.Tilde {
				panic("TODO")
			}
			termTy := c.ResolveType(term.Type)
			switch underTy := c.Under(termTy).(type) {
			case *tree.InterfaceType:
				next = c.InterfaceTypeSet(underTy)
			case *tree.TypeParam:
				if underTy.Bound != nil {
					next = c.InterfaceTypeSet(underTy.Bound)
				} else {
					next = TypeSet{Types: []tree.Type{underTy}, Universe: false}
				}
			default:
				next = TypeSet{Types: []tree.Type{termTy}, Universe: false}
			}
		} else {
			var types []tree.Type
			for _, term := range constraint.TypeElem.Union {
				if term.Tilde {
					panic("TODO")
				}
				termTy := c.ResolveType(term.Type)
				switch ty := c.Under(termTy).(type) {
				case *tree.InterfaceType:
					if len(ty.Methods) == 0 {
						spew.Dump(ty)
						panic("cannot make union of interface with methods")
					}
					spew.Dump(ty)
					panic("what")
				default:
					types = append(types, ty)
				}
			}
			next = TypeSet{Types: types, Universe: false}
		}
		typeset = c.Combine(typeset, next)
	}

	return typeset
}

func (c *Checker) TypeSet(con tree.TypeConstraint) TypeSet {
	if len(con.TypeElem.Union) == 1 {
		term := con.TypeElem.Union[0]
		if term.Tilde {
			panic("TODO")
		}
		termTy := c.ResolveType(term.Type)
		switch ty := c.Under(termTy).(type) {
		case *tree.InterfaceType:
			return c.InterfaceTypeSet(ty)
		case *tree.TypeParam:
			if ty.Bound != nil {
				return c.InterfaceTypeSet(ty.Bound)
			}
			return TypeSet{Types: []tree.Type{ty}, Universe: false}
		default:
			return TypeSet{Types: []tree.Type{ty}, Universe: false}
		}
	}
	var types []tree.Type
	for _, term := range con.TypeElem.Union {
		if term.Tilde {
			panic("TODO")
		}
		termTy := c.ResolveType(term.Type)
		switch ty := c.Under(termTy).(type) {
		case *tree.InterfaceType:
			if len(ty.Methods) == 0 {
				spew.Dump(ty)
				panic("cannot make union of interface with methods")
			}
		default:
			types = append(types, ty)
		}
	}
	return TypeSet{Types: types, Universe: false}
}
