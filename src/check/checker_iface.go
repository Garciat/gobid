package check

import "github.com/garciat/gobid/tree"

func (c *Checker) SimplifyInterface(ty *tree.InterfaceType) *tree.InterfaceType {
	if single, ok := c.IsSingleTypeUnion(ty); ok {
		switch single := single.(type) {
		case *tree.InterfaceType:
			return c.SimplifyInterface(single)
		case *tree.TypeName:
			singleRef := c.Lookup(single.Name)
			if singleRef, ok := singleRef.(*tree.InterfaceType); ok {
				return c.SimplifyInterface(singleRef)
			}
		}
	}
	return ty
}

func (c *Checker) IsSingleTypeUnion(ty *tree.InterfaceType) (tree.Type, bool) {
	tyset := c.InterfaceTypeSet(ty)
	if len(tyset.Methods) == 0 && len(tyset.Terms) == 1 && !tyset.Terms[0].Tilde {
		return tyset.Terms[0].Type, true
	}
	return nil, false
}
