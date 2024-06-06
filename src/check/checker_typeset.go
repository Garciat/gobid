package check

import (
	"github.com/davecgh/go-spew/spew"
	"github.com/garciat/gobid/tree"
)

type TypeSet struct {
	Methods  []*tree.MethodElem
	Terms    []TypeSetTerm
	Universe bool
}

func (ts *TypeSet) IsEmpty() bool {
	return len(ts.Terms) == 0 && !ts.Universe
}

func SingleTypeSet(ty tree.Type, tilde bool) TypeSet {
	return TypeSet{Terms: []TypeSetTerm{{Type: ty, Tilde: tilde}}, Universe: false}
}

type TypeSetTerm struct {
	Type  tree.Type
	Tilde bool
}

func (c *Checker) Combine(lhs, rhs TypeSet) TypeSet {
	result := TypeSet{
		Methods:  []*tree.MethodElem{},
		Terms:    []TypeSetTerm{},
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
		if len(lhs.Terms) != 0 {
			panic("weird")
		}
		if len(rhs.Terms) != 0 {
			panic("weird")
		}
	} else if lhs.Universe && !rhs.Universe {
		result.Terms = append(result.Terms, rhs.Terms...)
	} else if !lhs.Universe && rhs.Universe {
		result.Terms = append(result.Terms, lhs.Terms...)
	} else {
		// intersect
		for _, t := range rhs.Terms {
			for _, u := range lhs.Terms {
				if t.Tilde == u.Tilde && c.Identical(t.Type, u.Type) {
					result.Terms = append(result.Terms, t)
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
			termTy := c.ResolveType(term.Type)
			switch underTy := c.Under(termTy).(type) {
			case *tree.InterfaceType:
				next = c.InterfaceTypeSet(underTy)
			case *tree.TypeParam:
				if underTy.Bound != nil {
					next = c.InterfaceTypeSet(underTy.Bound)
				} else {
					next = SingleTypeSet(termTy, term.Tilde)
				}
			default:
				next = SingleTypeSet(termTy, term.Tilde)
			}
		} else {
			var types []TypeSetTerm
			for _, term := range constraint.TypeElem.Union {
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
					types = append(types, TypeSetTerm{Type: termTy, Tilde: term.Tilde})
				}
			}
			next = TypeSet{Terms: types, Universe: false}
		}
		typeset = c.Combine(typeset, next)
	}

	return typeset
}
