package check

import (
	"fmt"
	. "github.com/garciat/gobid/common"
	"github.com/garciat/gobid/tree"
	"strings"
)

// ========================

type TypeContext struct {
	ScopeKind ScopeKind
	Parent    *TypeContext
	Relations []Relation
}

func (c *TypeContext) String() string {
	parent := ""
	if c.Parent != nil {
		parent = fmt.Sprintf("%v || ", c.Parent)
	}
	parts := make([]string, 0, len(c.Relations))
	for _, rel := range c.Relations {
		parts = append(parts, fmt.Sprintf("%v", rel))
	}
	if len(parts) == 0 {
		return fmt.Sprintf("%v{}", parent)
	}
	return fmt.Sprintf("%v%v", parent, strings.Join(parts, ", "))
}

func (c *TypeContext) AddRelation(rel Relation) {
	c.Relations = append(c.Relations, rel)
}

func (c *TypeContext) Fork(scope ScopeKind) *TypeContext {
	return &TypeContext{
		ScopeKind: scope,
		Parent:    c,
		Relations: []Relation{},
	}

}

func (c *Checker) ApplySubstRelations(relations []Relation, learned Subst) []Relation {
	next := make([]Relation, 0, len(relations))

	for _, rel := range relations {
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

	return next
}

// ========================

type VarContext struct {
	ScopeKind ScopeKind
	Parent    *VarContext
	Types     map[Identifier]tree.Type
}

func NewVarContext() *VarContext {
	return &VarContext{
		Types: map[Identifier]tree.Type{},
	}
}

func (c *VarContext) Fork(scope ScopeKind) *VarContext {
	return &VarContext{
		ScopeKind: scope,
		Parent:    c,
		Types:     map[Identifier]tree.Type{},
	}
}

func (c *VarContext) Lookup(name Identifier) (tree.Type, bool) {
	ty, ok := c.Types[name]
	if ok {
		if ty, ok := ty.(*tree.ConstValueType); ok {
			return ty.Type, true
		}
		return ty, true
	}
	if c.Parent != nil {
		return c.Parent.Lookup(name)
	}
	return nil, false
}

func (c *VarContext) LookupConst(name Identifier) (tree.Expr, bool) {
	expr, ok := c.Types[name]
	if ok {
		if expr, ok := expr.(*tree.ConstValueType); ok {
			return expr.Value, true
		}
		panic(fmt.Errorf("not a constant: %v", name))
	}
	if c.Parent != nil {
		return c.Parent.LookupConst(name)
	}
	return nil, false
}

func (c *VarContext) Def(name Identifier, ty tree.Type) tree.Type {
	if name != IgnoreIdent {
		if _, ok := c.Types[name]; ok {
			panic(fmt.Errorf("redefined: %v", name))
		}
		c.Types[name] = ty
	}
	return ty
}

func (c *VarContext) DefBuiltinFunction(name string) tree.Type {
	return c.Def(NewIdentifier(name), &tree.BuiltinFunctionType{Name: name})
}

func (c *VarContext) DefType(name Identifier, ty tree.Type) tree.Type {
	if _, ok := ty.(*tree.TypeOfType); ok {
		panic("BUG")
	}
	return c.Def(name, &tree.TypeOfType{Type: ty})
}

func (c *VarContext) DefNamedType(ip ImportPath, name Identifier, under tree.Type) tree.Type {
	return c.DefType(name, &tree.NamedType{Package: ip, Name: name, Definition: under})
}

func (c *VarContext) DefBuiltinType(ty *tree.BuiltinType) tree.Type {
	return c.DefType(NewIdentifier(string(ty.Tag)), ty)
}

func (c *VarContext) Iter(f func(Identifier, tree.Type)) {
	if c.Parent != nil {
		c.Parent.Iter(f)
	}
	for name, ty := range c.Types {
		f(name, ty)
	}
}

// ========================
