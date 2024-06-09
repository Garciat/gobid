package check

import (
	"fmt"
	. "github.com/garciat/gobid/common"
	"github.com/garciat/gobid/tree"
	"strings"
)

type TypeContext struct {
	ScopeKind ScopeKind
	Parent    *TypeContext
	Relations []Relation
	Types     map[Identifier]tree.Type
}

func NewTypeContext() *TypeContext {
	return &TypeContext{
		Types: map[Identifier]tree.Type{},
	}
}

func (c *TypeContext) FormatRelations() string {
	parent := ""
	if c.Parent != nil {
		parent = fmt.Sprintf("%v || ", c.Parent.FormatRelations())
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
		Types:     map[Identifier]tree.Type{},
	}

}

func (c *TypeContext) Lookup(name Identifier) (tree.Type, bool) {
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

func (c *TypeContext) LookupConst(name Identifier) (tree.Expr, bool) {
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

func (c *TypeContext) Def(name Identifier, ty tree.Type) tree.Type {
	if name != IgnoreIdent {
		if _, ok := c.Types[name]; ok {
			panic(fmt.Errorf("redefined: %v", name))
		}
		c.Types[name] = ty
	}
	return ty
}

func (c *TypeContext) DefBuiltinFunction(name string) tree.Type {
	return c.Def(NewIdentifier(name), &tree.BuiltinFunctionType{Name: name})
}

func (c *TypeContext) DefType(name Identifier, ty tree.Type) tree.Type {
	if _, ok := ty.(*tree.TypeOfType); ok {
		panic("BUG")
	}
	return c.Def(name, &tree.TypeOfType{Type: ty})
}

func (c *TypeContext) DefNamedType(ip ImportPath, name Identifier, under tree.Type) tree.Type {
	return c.DefType(name, &tree.NamedType{Package: ip, Name: name, Definition: under})
}

func (c *TypeContext) DefBuiltinType(ty *tree.BuiltinType) tree.Type {
	return c.DefType(NewIdentifier(string(ty.Tag)), ty)
}

func (c *TypeContext) Iter(f func(Identifier, tree.Type)) {
	if c.Parent != nil {
		c.Parent.Iter(f)
	}
	for name, ty := range c.Types {
		f(name, ty)
	}
}
