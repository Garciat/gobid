package check

import (
	"fmt"
	"github.com/davecgh/go-spew/spew"
	"github.com/garciat/gobid/common"
	"github.com/garciat/gobid/tree"
	"strings"
)

func (c *Checker) MethodSet(ty tree.Type) ([]*tree.MethodElem, bool) {
	ty = c.ResolveType(ty)

	var pointerReceiver bool
	if pointerTy, ok := ty.(*tree.PointerType); ok {
		ty = pointerTy.BaseType
		pointerReceiver = true
	}

	switch ty := c.ResolveType(ty).(type) {
	case *tree.InterfaceType:
		return ty.Methods, false
	case *tree.NamedType:
		return c.NamedTypeMethods(ty), pointerReceiver
	case *tree.TypeApplication:
		named, subst := c.InstantiateType(ty)
		var methods []*tree.MethodElem
		for _, m := range c.NamedTypeMethods(named) {
			methods = append(methods, &tree.MethodElem{
				Name: m.Name,
				Type: c.ApplySubst(m.Type, subst).(*tree.FunctionType),
			})
		}
		return methods, pointerReceiver
	default:
		spew.Dump(ty)
		panic(fmt.Sprintf("type %v cannot have methods", ty))
	}
}

func (c *Checker) NamedTypeMethods(namedTy *tree.NamedType) []*tree.MethodElem {
	if interTy, ok := c.Under(namedTy).(*tree.InterfaceType); ok {
		return interTy.Methods
	}

	var methods []*tree.MethodElem

	c.VarCtx.Iter(func(name common.Identifier, ty tree.Type) {
		if strings.HasPrefix(name.Value, namedTy.Name.Value+".") {
			methodTy := ty.(*tree.MethodType)
			methods = append(methods, &tree.MethodElem{
				Name:            common.NewIdentifier(name.Value[len(namedTy.Name.Value)+1:]),
				Type:            methodTy.Type,
				PointerReceiver: methodTy.PointerReceiver,
			})
		}
	})

	return methods
}
