package tree

import (
	"fmt"
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

var TheVoidType = &VoidType{}

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

type ImportTypeName struct {
	TypeBase
	Import Type
	Name   Identifier
}

func (t *ImportTypeName) String() string {
	return fmt.Sprintf("%s.%s", t.Import, t.Name)
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

type ConstValueType struct {
	TypeBase
	Value ConstExpr
	Type  Type
}

func (t *ConstValueType) String() string {
	return fmt.Sprintf("const(%v)", t.Value)
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

var (
	UntypedConstantIntType     = &UntypedConstantType{Kind: UntypedConstantInt}
	UntypedConstantFloatType   = &UntypedConstantType{Kind: UntypedConstantFloat}
	UntypedConstantComplexType = &UntypedConstantType{Kind: UntypedConstantComplex}
	UntypedConstantRuneType    = &UntypedConstantType{Kind: UntypedConstantRune}
	UntypedConstantStringType  = &UntypedConstantType{Kind: UntypedConstantString}
	UntypedConstantBoolType    = &UntypedConstantType{Kind: UntypedConstantBool}
)

type UntypedConstantType struct {
	TypeBase
	Kind UntypedConstantKind
}

func (t *UntypedConstantType) DefaultType() *BuiltinType {
	switch t.Kind {
	case UntypedConstantInt:
		return BuiltinTypeInt
	case UntypedConstantFloat:
		return BuiltinTypeFloat64
	case UntypedConstantComplex:
		return BuiltinTypeComplex128
	case UntypedConstantRune:
		return BuiltinTypeRune
	case UntypedConstantString:
		return BuiltinTypeString
	case UntypedConstantBool:
		return BuiltinTypeBool
	default:
		panic("unreachable")
	}
}

func (t *UntypedConstantType) IsString() bool {
	return t.Kind == UntypedConstantString
}

func (t *UntypedConstantType) IsNumeric() bool {
	switch t.Kind {
	case UntypedConstantInt, UntypedConstantFloat, UntypedConstantComplex, UntypedConstantRune:
		return true
	default:
		return false
	}
}

func (t *UntypedConstantType) IsInteger() bool {
	return t.Kind == UntypedConstantInt || t.Kind == UntypedConstantRune
}

func (t *UntypedConstantType) IsFloat() bool {
	return t.Kind == UntypedConstantFloat
}

func (t *UntypedConstantType) IsComplex() bool {
	return t.Kind == UntypedConstantComplex
}

func (t *UntypedConstantType) IsAssignableTo(ty *BuiltinType) bool {
	switch t.Kind {
	case UntypedConstantInt:
		return ty.IsInteger() || ty.IsFloat() || ty.IsComplex()
	case UntypedConstantFloat:
		return ty.IsFloat()
	case UntypedConstantComplex:
		return ty.IsComplex()
	case UntypedConstantRune:
		return ty.IsInteger()
	case UntypedConstantString:
		return ty.IsString()
	case UntypedConstantBool:
		return ty.IsBoolean()
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

type BuiltinTypeTag string

const (
	BuiltinTypeTagBool          BuiltinTypeTag = "bool"
	BuiltinTypeTagUint8         BuiltinTypeTag = "uint8"
	BuiltinTypeTagUint16        BuiltinTypeTag = "uint16"
	BuiltinTypeTagUint32        BuiltinTypeTag = "uint32"
	BuiltinTypeTagUint64        BuiltinTypeTag = "uint64"
	BuiltinTypeTagInt8          BuiltinTypeTag = "int8"
	BuiltinTypeTagInt16         BuiltinTypeTag = "int16"
	BuiltinTypeTagInt32         BuiltinTypeTag = "int32"
	BuiltinTypeTagInt64         BuiltinTypeTag = "int64"
	BuiltinTypeTagFloat32       BuiltinTypeTag = "float32"
	BuiltinTypeTagFloat64       BuiltinTypeTag = "float64"
	BuiltinTypeTagComplex64     BuiltinTypeTag = "complex64"
	BuiltinTypeTagComplex128    BuiltinTypeTag = "complex128"
	BuiltinTypeTagString        BuiltinTypeTag = "string"
	BuiltinTypeTagInt           BuiltinTypeTag = "int"
	BuiltinTypeTagUint          BuiltinTypeTag = "uint"
	BuiltinTypeTagUintptr       BuiltinTypeTag = "uintptr"
	BuiltinTypeTagByte          BuiltinTypeTag = "byte"
	BuiltinTypeTagRune          BuiltinTypeTag = "rune"
	BuiltinTypeTagComparable    BuiltinTypeTag = "comparable"
	BuiltinTypeTagUnsafePointer BuiltinTypeTag = "unsafe.Pointer"
)

var (
	BuiltinTypeBool          = &BuiltinType{Tag: BuiltinTypeTagBool}
	BuiltinTypeUint8         = &BuiltinType{Tag: BuiltinTypeTagUint8}
	BuiltinTypeUint16        = &BuiltinType{Tag: BuiltinTypeTagUint16}
	BuiltinTypeUint32        = &BuiltinType{Tag: BuiltinTypeTagUint32}
	BuiltinTypeUint64        = &BuiltinType{Tag: BuiltinTypeTagUint64}
	BuiltinTypeInt8          = &BuiltinType{Tag: BuiltinTypeTagInt8}
	BuiltinTypeInt16         = &BuiltinType{Tag: BuiltinTypeTagInt16}
	BuiltinTypeInt32         = &BuiltinType{Tag: BuiltinTypeTagInt32}
	BuiltinTypeInt64         = &BuiltinType{Tag: BuiltinTypeTagInt64}
	BuiltinTypeFloat32       = &BuiltinType{Tag: BuiltinTypeTagFloat32}
	BuiltinTypeFloat64       = &BuiltinType{Tag: BuiltinTypeTagFloat64}
	BuiltinTypeComplex64     = &BuiltinType{Tag: BuiltinTypeTagComplex64}
	BuiltinTypeComplex128    = &BuiltinType{Tag: BuiltinTypeTagComplex128}
	BuiltinTypeString        = &BuiltinType{Tag: BuiltinTypeTagString}
	BuiltinTypeInt           = &BuiltinType{Tag: BuiltinTypeTagInt}
	BuiltinTypeUint          = &BuiltinType{Tag: BuiltinTypeTagUint}
	BuiltinTypeUintptr       = &BuiltinType{Tag: BuiltinTypeTagUintptr}
	BuiltinTypeByte          = &BuiltinType{Tag: BuiltinTypeTagByte}
	BuiltinTypeRune          = &BuiltinType{Tag: BuiltinTypeTagRune}
	BuiltinTypeComparable    = &BuiltinType{Tag: BuiltinTypeTagComparable}
	BuiltinTypeUnsafePointer = &BuiltinType{Tag: BuiltinTypeTagUnsafePointer}
)

type BuiltinType struct {
	TypeBase
	Tag BuiltinTypeTag
}

func (t *BuiltinType) String() string {
	return fmt.Sprintf("%sᵢ", t.Tag)
}

func (t *BuiltinType) IsBoolean() bool {
	return t.Tag == BuiltinTypeTagBool
}

func (t *BuiltinType) IsString() bool {
	return t.Tag == BuiltinTypeTagString
}

func (t *BuiltinType) IsNumeric() bool {
	switch t.Tag {
	case BuiltinTypeTagUint8, BuiltinTypeTagUint16, BuiltinTypeTagUint32, BuiltinTypeTagUint64,
		BuiltinTypeTagInt8, BuiltinTypeTagInt16, BuiltinTypeTagInt32, BuiltinTypeTagInt64,
		BuiltinTypeTagFloat32, BuiltinTypeTagFloat64, BuiltinTypeTagComplex64, BuiltinTypeTagComplex128,
		BuiltinTypeTagInt, BuiltinTypeTagUint, BuiltinTypeTagUintptr, BuiltinTypeTagByte, BuiltinTypeTagRune:
		return true
	default:
		return false
	}
}

func (t *BuiltinType) IsInteger() bool {
	switch t.Tag {
	case BuiltinTypeTagUint8, BuiltinTypeTagUint16, BuiltinTypeTagUint32, BuiltinTypeTagUint64,
		BuiltinTypeTagInt8, BuiltinTypeTagInt16, BuiltinTypeTagInt32, BuiltinTypeTagInt64,
		BuiltinTypeTagInt, BuiltinTypeTagUint, BuiltinTypeTagUintptr, BuiltinTypeTagByte, BuiltinTypeTagRune:
		return true
	default:
		return false
	}
}

func (t *BuiltinType) IsFloat() bool {
	switch t.Tag {
	case BuiltinTypeTagFloat32, BuiltinTypeTagFloat64:
		return true
	default:
		return false
	}
}

func (t *BuiltinType) IsComplex() bool {
	switch t.Tag {
	case BuiltinTypeTagComplex64, BuiltinTypeTagComplex128:
		return true
	default:
		return false
	}
}

func (t *BuiltinType) IsConversibleTo(target *BuiltinType) bool {
	switch {
	case t.IsString() && target.IsString():
		return true
	case t.IsBoolean() && target.IsBoolean():
		return true
	case t.IsInteger() && target.IsInteger():
		return true
	case t.IsFloat() && target.IsFloat():
		return true
	case t.IsComplex() || target.IsComplex():
		return false
	case t.IsInteger() && target.IsFloat():
		return true
	case t.IsFloat() && target.IsInteger():
		return true
	case t.IsInteger() && target.IsString():
		return true
	case t.Tag == BuiltinTypeTagUnsafePointer && target.Tag == BuiltinTypeTagUintptr:
		return true
	default:
		return false
	}
}

type MethodsByName = Map[Identifier, *MethodElem]

type NamedType struct {
	TypeBase
	Package    ImportPath
	Name       Identifier
	Definition Type
	Methods    MethodsByName
}

func (t *NamedType) SameType(other *NamedType) bool {
	return t.Package == other.Package && t.Name == other.Name
}

func (t *NamedType) String() string {
	return fmt.Sprintf("type %s %v", t.Name.Value, t.Definition)
}

type TypeParam struct {
	TypeBase
	Name  Identifier
	Bound *InterfaceType
}

func (t *TypeParam) String() string {
	return fmt.Sprintf("%sₚ", t.Name.Value)
}

type FreeTypeVar struct {
	TypeBase
	Name Identifier
}

func (t *FreeTypeVar) String() string {
	return fmt.Sprintf("%sᶠ", t.Name.Value)
}

type TypeApplication struct {
	TypeBase
	Type Type
	Args []Type
}

func (t *TypeApplication) String() string {
	parts := make([]string, 0, len(t.Args))
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
	return fmt.Sprintf("[%v]%v", t.Len, t.ElemType)
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

func (t *FunctionType) WithoutTypeParams() *FunctionType {
	return &FunctionType{
		Signature: t.Signature.WithoutTypeParams(),
	}
}

func (t *FunctionType) WithTypeParams(names ...string) *FunctionType {
	return &FunctionType{
		Signature: t.Signature.WithTypeParams(names...),
	}
}

func (t *FunctionType) String() string {
	return fmt.Sprintf("func%v", t.Signature)
}

// TupleType represents the type of multiple results.
type TupleType struct {
	TypeBase
	Elems []Type
}

func (t *TupleType) String() string {
	parts := make([]string, 0, len(t.Elems))
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

func (s *Signature) WithoutTypeParams() *Signature {
	if len(s.TypeParams.Params) == 0 {
		return s
	}
	return &Signature{
		TypeParams: &TypeParamList{},
		Params:     s.Params,
		Results:    s.Results,
	}
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

func (s *Signature) GetVariadicParam() (*ParameterDecl, int) {
	for i, param := range s.Params.Params {
		if param.Variadic {
			return param, i
		}
	}
	return nil, -1
}

func (s *Signature) HasNamedResults() bool {
	// TODO all should be named?
	for _, param := range s.Results.Params {
		if param.Name != IgnoreIdent {
			return true
		}
	}
	return false
}

func (s *Signature) String() string {
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
	parts := make([]string, 0, len(l.Params))
	for _, param := range l.Params {
		parts = append(parts, fmt.Sprintf("%v %v", param.Name, param.Constraint))
	}
	return strings.Join(parts, ", ")
}

type TypeParamDecl struct {
	Name       Identifier
	Constraint *InterfaceType
}

func (p *TypeParamDecl) AsTypeParam() *TypeParam {
	return &TypeParam{Name: p.Name, Bound: p.Constraint}
}

type ParameterList struct {
	Params []*ParameterDecl
}

func (p ParameterList) String() string {
	parts := make([]string, 0, len(p.Params))
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
}

func (t *StructType) Embeds() []Type {
	var embeds []Type
	for _, field := range t.Fields {
		if field.Name.Value == "" {
			embeds = append(embeds, field.Type)
		}
	}
	return embeds
}

func (t *StructType) String() string {
	parts := make([]string, 0, len(t.Fields))
	for _, field := range t.Fields {
		parts = append(parts, fmt.Sprintf("%v %v", field.Name, field.Type))
	}
	return fmt.Sprintf("struct{%v}", strings.Join(parts, "; "))
}

type FieldDecl struct {
	Name Identifier // empty for embedded types
	Type Type
}

type PointerType struct {
	TypeBase
	ElemType Type
}

func (t *PointerType) String() string {
	return fmt.Sprintf("*%v", t.ElemType)
}

type InterfaceType struct {
	TypeBase
	Methods     MethodsByName
	Constraints []*TypeConstraint
}

func (t *InterfaceType) String() string {
	parts := make([]string, 0, len(t.Methods)+len(t.Constraints))
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
	KeyType   Type
	ValueType Type
}

func (t *MapType) String() string {
	return fmt.Sprintf("map[%v]%v", t.KeyType, t.ValueType)
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
	parts := make([]string, 0, len(e.Union))
	for _, term := range e.Union {
		parts = append(parts, term.String())
	}
	return strings.Join(parts, " | ")
}

type TypeConstraint struct {
	TypeElem *TypeElem
}

func (c TypeConstraint) String() string {
	return fmt.Sprintf("%v", c.TypeElem)
}
