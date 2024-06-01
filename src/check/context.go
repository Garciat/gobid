package check

import (
	"fmt"
	. "github.com/garciat/gobid/common"
	"github.com/garciat/gobid/tree"
	"strings"
)

// ========================

type TypeContext struct {
	Parent    *TypeContext
	Relations []Relation
}

func (c TypeContext) String() string {
	parent := ""
	if c.Parent != nil {
		parent = fmt.Sprintf("%v || ", c.Parent)
	}
	parts := []string{}
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

func (c *TypeContext) AddEq(left, right tree.Type) {
	c.AddRelation(RelationEq{Left: left, Right: right})
}

func (c *TypeContext) Fork() *TypeContext {
	return &TypeContext{Parent: c, Relations: []Relation{}}

}

// ========================

type VarContext struct {
	ScopeKind ScopeKind
	Parent    *VarContext
	Types     map[Identifier]tree.Type
}

func EmptyVarContext() *VarContext {
	return &VarContext{Types: map[Identifier]tree.Type{}}
}

func (c VarContext) Format() string {
	parts := []string{}
	c.Iter(func(name Identifier, ty tree.Type) {
		parts = append(parts, fmt.Sprintf("%v: %v", name, ty))
	})
	return strings.Join(parts, "\n")
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

func (c *VarContext) DefNamedType(name Identifier, under tree.Type) tree.Type {
	return c.DefType(name, &tree.NamedType{Name: name, Type: under})
}

func (c *VarContext) DefBuiltinType(name string) tree.Type {
	return c.DefType(NewIdentifier(name), tree.NewBuiltinType(name))
}

func (c *VarContext) DefBuiltinNumericType(name string) tree.Type {
	return c.DefType(NewIdentifier(name), tree.NewBuiltinNumericType(name))
}

func (c *VarContext) Lookup(name Identifier) (tree.Type, bool) {
	ty, ok := c.Types[name]
	if !ok && c.Parent != nil {
		return c.Parent.Lookup(name)
	}
	return ty, ok
}

func (c *VarContext) Fork(kind ScopeKind) *VarContext {
	return &VarContext{
		ScopeKind: kind,
		Parent:    c,
		Types:     map[Identifier]tree.Type{},
	}
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
