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
		switch imp := ty.Import.(type) {
		case *tree.TypeName:
			return c.Lookup(imp.Name)
		case *tree.ImportType:
			return imp
		case *tree.ImportRef:
			return c.PackageLookup(imp.ImportPath, ty.Name)
		default:
			spew.Dump(imp)
			panic(fmt.Errorf("not an import: %v", imp))
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
		return c.Under(ty.Type)
	case *tree.TypeApplication:
		// TODO: pre apply?
		return c.TypeApplication(ty) // TODO crazy?
	default:
		return ty
	}
}
