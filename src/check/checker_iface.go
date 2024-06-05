package check

import "github.com/garciat/gobid/tree"

func (c *Checker) SimplifyInterface(ty *tree.InterfaceType) *tree.InterfaceType {
	if single, ok := IsSingleTypeUnion(ty); ok {
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

func IsSingleTypeUnion(ty *tree.InterfaceType) (tree.Type, bool) {
	if len(ty.Methods) == 0 && len(ty.Constraints) == 1 && len(ty.Constraints[0].TypeElem.Union) == 1 {
		return ty.Constraints[0].TypeElem.Union[0].Type, true
	}
	return nil, false
}
