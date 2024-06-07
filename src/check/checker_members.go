package check

import (
	"fmt"
	"github.com/davecgh/go-spew/spew"
	"github.com/garciat/gobid/common"
	"github.com/garciat/gobid/tree"
)

type MemberSet = common.Map[common.Identifier, []TypeMember]

type TypeMember interface {
	_TypeMember()
}

type TypeMemberBase struct{}

func (*TypeMemberBase) _TypeMember() {}

type FieldMember struct {
	TypeMemberBase
	Field *tree.FieldDecl
}

type MethodMember struct {
	TypeMemberBase
	Method *tree.MethodElem
}

func (c *Checker) GetMemberType(ty tree.Type, name common.Identifier) (tree.Type, error) {
	members := c.Members(ty)
	options := members[name]
	if len(options) == 0 {
		return nil, fmt.Errorf("type %v does not have member %q", ty, name)
	}
	if len(options) > 1 {
		return nil, fmt.Errorf("type %v has ambiguous members %q", ty, name)
	}
	switch member := options[0].(type) {
	case *FieldMember:
		return member.Field.Type, nil
	case *MethodMember:
		return member.Method.Type, nil
	default:
		spew.Dump(member)
		panic("unreachable")
	}
}

func (c *Checker) Members(ty tree.Type) MemberSet {
	ty = c.ResolveType(ty)

	if pointerTy, ok := ty.(*tree.PointerType); ok {
		ty = c.ResolveType(pointerTy.ElemType)
	}

	embedded := MemberSet{}
	members := MemberSet{}

	switch underTy := c.Under(ty).(type) {
	case *tree.StructType:
		for _, embedTy := range underTy.Embeds() {
			for name, member := range c.Members(embedTy) {
				embedded[name] = append(embedded[name], member...)
			}

			fieldName := c.GetStructEmbedFieldName(embedTy)
			members[fieldName] = []TypeMember{&FieldMember{Field: &tree.FieldDecl{
				Name: fieldName,
				Type: embedTy,
			}}}
		}
		for _, field := range underTy.Fields {
			members[field.Name] = []TypeMember{&FieldMember{Field: field}}
		}
	case *tree.InterfaceType:
		typeset := c.InterfaceTypeSet(underTy) // TODO should not allow general interfaces?
		if !typeset.Universe {
			panic("interface type set is not universe?")
		}
		for _, method := range typeset.Methods {
			members[method.Name] = []TypeMember{&MethodMember{Method: method}}
		}
	}

	switch ty := ty.(type) {
	case *tree.NamedType:
		for _, method := range ty.Methods {
			if members.Contains(method.Name) {
				panic(fmt.Errorf("type %v has both field and method called %q", ty, method.Name))
			}
			members[method.Name] = []TypeMember{&MethodMember{Method: method}}
		}
	case *tree.TypeApplication:
		named, subst := c.InstantiateType(ty)
		for _, method := range named.Methods {
			members[method.Name] = []TypeMember{
				&MethodMember{
					Method: &tree.MethodElem{
						Name: method.Name,
						Type: c.ApplySubst(method.Type, subst).(*tree.FunctionType),
					},
				},
			}
		}
	}

	final := MemberSet{}
	final.Merge(embedded)
	final.Merge(members) // overwrite embedded members with local members

	return final
}

func (c *Checker) GetStructEmbedFieldName(ty tree.Type) common.Identifier {
	// TODO disallow type parameters
	if pointerTy, ok := ty.(*tree.PointerType); ok {
		ty = pointerTy.ElemType
	}

	if tyAppTy, ok := ty.(*tree.TypeApplication); ok {
		ty = tyAppTy.Type
	}

	switch ty := ty.(type) {
	case *tree.TypeName:
		return ty.Name
	case *tree.PackageTypeName:
		return ty.Name
	case *tree.ImportTypeName:
		return ty.Name
	default:
		spew.Dump(ty)
		panic("unreachable")
	}
}

func (c *Checker) CheckMethodsSatisfy(ty tree.Type, target tree.MethodsByName) error {
	pointerReceiver := c.IsPointerType(ty) // TODO should check IsAddressable instead
	members := c.Members(ty)

	var pointerReceiverMethods []*tree.MethodElem

	for _, method := range target {
		options := members[method.Name]
		if len(options) == 0 {
			return fmt.Errorf("type %v does not have method %v", ty, method.Name)
		}
		if len(options) > 1 {
			return fmt.Errorf("type %v has ambiguous methods %v (embedded)", ty, method.Name)
		}
		option, ok := options[0].(*MethodMember)
		if !ok {
			return fmt.Errorf("type %v member %v is a field", ty, method.Name)
		}
		if option.Method.PointerReceiver {
			pointerReceiverMethods = append(pointerReceiverMethods, option.Method)
		}
		if !c.Identical(method.Type, option.Method.Type) {
			return fmt.Errorf("type %v method %v has type mismatch", ty, method.Name)
		}
		// OK
	}

	if !pointerReceiver {
		for _, method := range pointerReceiverMethods {
			return fmt.Errorf("type %v method %v has pointer receiver", ty, method.Name)
		}
	}

	return nil
}
