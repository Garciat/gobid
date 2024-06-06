package check

import (
	"fmt"
	"github.com/davecgh/go-spew/spew"
	"github.com/garciat/gobid/tree"
)

func (c *Checker) ResolveValue(ty tree.Type) tree.Type {
	switch ty := ty.(type) {
	case *tree.TypeName:
		return c.Lookup(ty.Name)
	case *tree.PackageTypeName:
		return c.PackageLookup(ty.Path, ty.Name)
	case *tree.ImportTypeName:
		switch impTy := ty.Import.(type) {
		case *tree.TypeName:
			imp, ok := c.Lookup(impTy.Name).(*tree.ImportType)
			if !ok {
				panic(fmt.Errorf("not an import: %v", imp))
			}
			return c.PackageLookup(imp.ImportPath, ty.Name)
		case *tree.ImportRef:
			return c.PackageLookup(impTy.ImportPath, ty.Name)
		default:
			spew.Dump(impTy)
			panic(fmt.Errorf("not an import: %v", impTy))
		}
	default:
		return ty
	}
}

func (c *Checker) ResolveType(ty tree.Type) tree.Type {
	var valueTy tree.Type
	switch ty := ty.(type) {
	case *tree.TypeName:
		valueTy = c.ResolveValue(ty)
	case *tree.PackageTypeName:
		valueTy = c.ResolveValue(ty)
	case *tree.ImportTypeName:
		valueTy = c.ResolveValue(ty)
	case *tree.TypeOfType:
		return ty.Type
	default:
		return ty
	}

	switch valueTy := valueTy.(type) {
	case *tree.TypeOfType:
		return valueTy.Type
	default:
		panic(fmt.Errorf("not a type: %v", valueTy))
	}
}

func (c *Checker) Under(ty tree.Type) tree.Type {
	switch ty := ty.(type) {
	case *tree.NamedType:
		return c.Under(c.ResolveType(ty.Definition))
	case *tree.TypeApplication:
		// TODO: pre apply?
		return c.TypeApplication(ty) // TODO crazy?
	default:
		return ty
	}
}
