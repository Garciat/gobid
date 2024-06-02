package common

var IgnoreIdent = Identifier{Value: "_"}

type Identifier struct {
	Value string
}

func (i Identifier) String() string {
	return i.Value
}

func NewIdentifier(name string) Identifier {
	return Identifier{name}
}

type DeclKind int

const (
	DeclKindImport DeclKind = iota
	DeclKindConst
	DeclKindVar
	DeclKindType
	DeclKindAlias
	DeclKindFunc
	DeclKindMethod
	DeclKindTypeParam
	DeclKindParam
)

func (k DeclKind) String() string {
	switch k {
	case DeclKindImport:
		return "DeclKindImport"
	case DeclKindConst:
		return "DeclKindConst"
	case DeclKindVar:
		return "DeclKindVar"
	case DeclKindType:
		return "DeclKindType"
	case DeclKindAlias:
		return "DeclKindAlias"
	case DeclKindFunc:
		return "DeclKindFunc"
	case DeclKindMethod:
		return "DeclKindMethod"
	case DeclKindTypeParam:
		return "DeclKindTypeParam"
	case DeclKindParam:
		return "DeclKindParam"
	default:
		panic("unreachable")
	}
}
