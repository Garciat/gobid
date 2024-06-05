package tree

import (
	"fmt"
	"github.com/davecgh/go-spew/spew"
	"slices"
	"strings"

	. "github.com/garciat/gobid/common"
)

type Type interface {
	Node
	_Type()
}

type TypeBase struct {
	NodeBase
}

func (*TypeBase) _Type() {}

type VoidType struct {
	TypeBase
}

type BottomType struct {
	TypeBase
}

type TypeName struct {
	TypeBase
	Name Identifier
}

func (t *TypeName) String() string {
	return fmt.Sprintf("%sₙ", t.Name.Value)
}

type PackageTypeName struct {
	TypeBase
	Path ImportPath
	Name Identifier
}

func (t *PackageTypeName) String() string {
	return fmt.Sprintf(`"%s".%s`, t.Path, t.Name)
}

type TypeOfType struct {
	TypeBase
	Type Type
}

func (t *TypeOfType) String() string {
	return fmt.Sprintf("type(%v)", t.Type)
}

type NilType struct {
	TypeBase
}

func (*NilType) String() string {
	return "type(nil)"
}

type UntypedConstantKind int

const (
	UntypedConstantInt UntypedConstantKind = iota
	UntypedConstantFloat
	UntypedConstantComplex
	UntypedConstantRune
	UntypedConstantString
	UntypedConstantBool
)

func (k UntypedConstantKind) String() string {
	switch k {
	case UntypedConstantInt:
		return "int"
	case UntypedConstantFloat:
		return "float"
	case UntypedConstantComplex:
		return "complex"
	case UntypedConstantRune:
		return "rune"
	case UntypedConstantString:
		return "string"
	case UntypedConstantBool:
		return "bool"
	default:
		panic("unreachable")
	}
}

type UntypedConstantType struct {
	TypeBase
	Kind UntypedConstantKind
}

func UntypedInt() *UntypedConstantType {
	return &UntypedConstantType{Kind: UntypedConstantInt}
}

func UntypedString() *UntypedConstantType {
	return &UntypedConstantType{Kind: UntypedConstantString}
}

func (t *UntypedConstantType) IsNumeric() bool {
	switch t.Kind {
	case UntypedConstantInt, UntypedConstantFloat, UntypedConstantComplex, UntypedConstantRune:
		return true
	default:
		return false
	}
}

func (t *UntypedConstantType) IsCompatible(builtin string) bool {
	switch t.Kind {
	case UntypedConstantInt:
		return slices.Contains([]string{"int", "int8", "int16", "int32", "int64", "uint", "uint8", "uint16", "uint32", "uint64", "uintptr"}, builtin)
	case UntypedConstantFloat:
		return slices.Contains([]string{"float32", "float64"}, builtin)
	case UntypedConstantComplex:
		return slices.Contains([]string{"complex64", "complex128"}, builtin)
	case UntypedConstantRune:
		return slices.Contains([]string{"rune"}, builtin)
	case UntypedConstantString:
		return slices.Contains([]string{"string"}, builtin)
	case UntypedConstantBool:
		return slices.Contains([]string{"bool"}, builtin)
	default:
		panic("unreachable")
	}
}

func (t *UntypedConstantType) String() string {
	return fmt.Sprintf("untyped(%s)", t.Kind.String())
}

type MethodType struct {
	TypeBase
	PointerReceiver bool
	Type            *FunctionType
}

func (t *MethodType) String() string {
	if t.PointerReceiver {
		return fmt.Sprintf("method (*) %v", t.Type)
	}
	return fmt.Sprintf("method (_) %v", t.Type)
}

type ImportType struct {
	TypeBase
	ImportPath ImportPath
}

func (t *ImportType) String() string {
	return fmt.Sprintf("import(%v)", t.ImportPath)
}

type TypeBuiltin struct {
	TypeBase
	Name      Identifier
	IsNumeric bool
}

func NewBuiltinType(name string) *TypeBuiltin {
	return &TypeBuiltin{Name: Identifier{Value: name}}
}

func NewBuiltinNumericType(name string) *TypeBuiltin {
	return &TypeBuiltin{Name: Identifier{Value: name}, IsNumeric: true}
}

func (t *TypeBuiltin) String() string {
	return fmt.Sprintf("%sᵢ", t.Name.Value)
}

type NamedType struct {
	TypeBase
	Name Identifier
	Type Type
}

func (t *NamedType) String() string {
	return fmt.Sprintf("type %s %v", t.Name.Value, t.Type)
}

type TypeParam struct {
	TypeBase
	Name  Identifier
	Bound *InterfaceType
}

func (t *TypeParam) String() string {
	return fmt.Sprintf("%sₚ", t.Name.Value)
}

type QualIdentifier struct {
	TypeBase
	Package string // could be empty
	Name    Identifier
}

func (i QualIdentifier) String() string {
	if i.Package == "" {
		return i.Name.String()
	}
	return fmt.Sprintf("%s.%s", i.Package, i.Name)
}

type TypeApplication struct {
	TypeBase
	Type Type
	Args []Type
}

func (t *TypeApplication) String() string {
	parts := []string{}
	for _, arg := range t.Args {
		parts = append(parts, fmt.Sprintf("%v", arg))
	}
	return fmt.Sprintf("%v[%s]", t.Type, strings.Join(parts, ", "))
}

type ArrayType struct {
	TypeBase
	ElemType Type
	Len      Expr
}

func (t *ArrayType) String() string {
	return fmt.Sprintf("[%v]%v", spew.Sdump(t.Len), t.ElemType)
}

type BuiltinFunctionType struct {
	TypeBase
	Name string
}

func (t *BuiltinFunctionType) String() string {
	return fmt.Sprintf("builtin(%v)", t.Name)
}

type FunctionType struct {
	TypeBase
	Signature *Signature
}

func (s *FunctionType) WithTypeParams(names ...string) *FunctionType {
	return &FunctionType{
		Signature: s.Signature.WithTypeParams(names...),
	}
}

func (t *FunctionType) String() string {
	return fmt.Sprintf("func%v", t.Signature)
}

// special for function return types
type TupleType struct {
	TypeBase
	Elems []Type
}

func (t *TupleType) String() string {
	parts := []string{}
	for _, elem := range t.Elems {
		parts = append(parts, fmt.Sprintf("%v", elem))
	}
	return fmt.Sprintf("(%v)", strings.Join(parts, ", "))
}

type Signature struct {
	TypeParams *TypeParamList
	Params     *ParameterList
	Results    *ParameterList
}

func (s *Signature) WithTypeParams(names ...string) *Signature {
	if len(s.TypeParams.Params) > 0 {
		panic("already has type params")
	}
	typeParams := &TypeParamList{}
	for _, name := range names {
		typeParams.Params = append(typeParams.Params, &TypeParamDecl{
			Name:       NewIdentifier(name),
			Constraint: EmptyInterface(),
		})
	}
	return &Signature{TypeParams: typeParams, Params: s.Params, Results: s.Results}
}

func (s Signature) GetVariadicParam() (*ParameterDecl, int) {
	for i, param := range s.Params.Params {
		if param.Variadic {
			return param, i
		}
	}
	return nil, -1
}

func (s Signature) HasNamedResults() bool {
	// TODO all should be named?
	for _, param := range s.Results.Params {
		if param.Name != IgnoreIdent {
			return true
		}
	}
	return false
}

func (s Signature) String() string {
	if len(s.TypeParams.Params) == 0 {
		return fmt.Sprintf("(%v) (%v)", s.Params, s.Results)
	} else {
		return fmt.Sprintf("[%v](%v) (%v)", s.TypeParams, s.Params, s.Results)
	}
}

type TypeParamList struct {
	Params []*TypeParamDecl
}

func (l TypeParamList) String() string {
	parts := []string{}
	for _, param := range l.Params {
		parts = append(parts, fmt.Sprintf("%v %v", param.Name, param.Constraint))
	}
	return strings.Join(parts, ", ")
}

type TypeParamDecl struct {
	Name       Identifier
	Constraint *InterfaceType
}

type ParameterList struct {
	Params []*ParameterDecl
}

func (p ParameterList) String() string {
	parts := []string{}
	for _, param := range p.Params {
		if param.Variadic {
			parts = append(parts, fmt.Sprintf("%v ...%v", param.Name, param.Type))
		} else {
			parts = append(parts, fmt.Sprintf("%v %v", param.Name, param.Type))
		}
	}
	return strings.Join(parts, ", ")
}

type ParameterDecl struct {
	Name     Identifier
	Type     Type
	Variadic bool // only for last parameter :')
}

type StructType struct {
	TypeBase
	Fields []*FieldDecl
	Embeds []Type
}

func (t *StructType) String() string {
	parts := []string{}
	for _, field := range t.Fields {
		parts = append(parts, fmt.Sprintf("%v %v", field.Name, field.Type))
	}
	return fmt.Sprintf("struct{%v}", strings.Join(parts, "; "))
}

type FieldDecl struct {
	Name Identifier
	Type Type
}

type PointerType struct {
	TypeBase
	BaseType Type // TODO rename to ElemType
}

func (t *PointerType) String() string {
	return fmt.Sprintf("*%v", t.BaseType)
}

type InterfaceType struct {
	TypeBase
	Methods     []*MethodElem
	Constraints []*TypeConstraint
}

func (t *InterfaceType) String() string {
	parts := []string{}
	for _, m := range t.Methods {
		parts = append(parts, m.String())
	}
	for _, c := range t.Constraints {
		parts = append(parts, c.String())
	}

	return fmt.Sprintf("interface{%v}", strings.Join(parts, "; "))
}

func EmptyInterface() *InterfaceType {
	return &InterfaceType{Methods: nil, Constraints: nil}
}

type MethodElem struct {
	Name            Identifier
	PointerReceiver bool // matters only for NamedType
	Type            *FunctionType
}

func (m MethodElem) String() string {
	return fmt.Sprintf("%v%v", m.Name, m.Type.Signature)
}

type SliceType struct {
	TypeBase
	ElemType Type
}

func (t *SliceType) String() string {
	return fmt.Sprintf("[]%v", t.ElemType)
}

type MapType struct {
	TypeBase
	KeyType  Type
	ElemType Type
}

func (t *MapType) String() string {
	return fmt.Sprintf("map[%v]%v", t.KeyType, t.ElemType)
}

type ChannelType struct {
	TypeBase
	ElemType Type
	Dir      ChannelDir
}

func (t *ChannelType) String() string {
	dir := ""
	switch t.Dir {
	case ChannelDirSend:
		dir = "<-chan"
	case ChannelDirRecv:
		dir = "chan<-"
	case ChannelDirBoth:
		dir = "chan"
	}
	return fmt.Sprintf("%v %v", dir, t.ElemType)
}

type GenericType struct {
	TypeBase
	TypeParams *TypeParamList
	Type       Type
}

func (t *GenericType) String() string {
	return fmt.Sprintf("∀[%v]%v", t.TypeParams, t.Type)
}

type ChannelDir int

const (
	ChannelDirSend ChannelDir = iota
	ChannelDirRecv
	ChannelDirBoth
)

// ========================

type TypeTerm struct {
	Type  Type
	Tilde bool
}

func (t TypeTerm) String() string {
	if t.Tilde {
		return fmt.Sprintf("~%v", t.Type)
	}
	return fmt.Sprintf("%v", t.Type)
}

type TypeElem struct {
	Union []*TypeTerm
}

func (e TypeElem) String() string {
	parts := []string{}
	for _, term := range e.Union {
		parts = append(parts, term.String())
	}
	return strings.Join(parts, "|")
}

type TypeConstraint struct {
	TypeElem *TypeElem
}

func (c TypeConstraint) String() string {
	return fmt.Sprintf("%v", c.TypeElem)
}
