package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os/exec"
	"path/filepath"
	"reflect"
	"slices"
	"strings"

	"github.com/davecgh/go-spew/spew"
)

// ========================

func Ptr[T any](x T) *T {
	return &x
}

// ========================

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

// ========================

type Type interface {
	_Type()
}

type TypeBase struct{}

func (*TypeBase) _Type() {}

type BottomType struct {
	TypeBase
}

type TypeOfType struct {
	TypeBase
	Type Type
}

func (t *TypeOfType) String() string {
	return fmt.Sprintf("type(%v)", t.Type)
}

type LazyType struct {
	TypeBase
	Expr Expr
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

type ImportType struct {
	TypeBase
	Decl  ImportDecl
	Scope VarContext
}

func (t *ImportType) String() string {
	if t.Decl.Alias != nil {
		return fmt.Sprintf("import(\"%v\" as %v)", t.Decl.Path, t.Decl.Alias)
	}
	return fmt.Sprintf("import(%v)", t.Decl.Path)
}

type TypeBuiltin struct {
	TypeBase
	Name Identifier
}

func NewBuiltinType(name string) *TypeBuiltin {
	return &TypeBuiltin{Name: Identifier{Value: name}}
}

func (t *TypeBuiltin) String() string {
	return fmt.Sprintf("%sᵢ", t.Name.Value)
}

type TypeName struct {
	TypeBase
	Name Identifier
}

func (t *TypeName) String() string {
	return fmt.Sprintf("%sₙ", t.Name.Value)
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
	Package string // can be empty
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
	ID   QualIdentifier
	Args []Type
}

func (t *TypeApplication) String() string {
	parts := []string{}
	for _, arg := range t.Args {
		parts = append(parts, fmt.Sprintf("%v", arg))
	}
	return fmt.Sprintf("%v[%s]", t.ID, strings.Join(parts, ", "))
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
	Signature Signature
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
	TypeParams TypeParamList
	Params     ParameterList
	Results    ParameterList
}

func (s Signature) GetVariadicParam() (ParameterDecl, int) {
	for i, param := range s.Params.Params {
		if param.Variadic {
			return param, i
		}
	}
	return ParameterDecl{}, -1
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

type ParameterList struct {
	Params []ParameterDecl
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
	Fields []FieldDecl
	// TODO: embedded fields
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
	Methods     []MethodElem
	Constraints []TypeConstraint
	// TODO: embedded interfaces
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

func EmptyInterface() Type {
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
	TypeParams TypeParamList
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
	Union []TypeTerm
}

func (e TypeElem) String() string {
	parts := []string{}
	for _, term := range e.Union {
		parts = append(parts, term.String())
	}
	return strings.Join(parts, "|")
}

type TypeConstraint struct {
	TypeElem TypeElem
}

func (c TypeConstraint) String() string {
	return fmt.Sprintf("%v", c.TypeElem)
}

// ========================

type BinaryOp int

const (
	BinaryOpAdd BinaryOp = iota
	BinaryOpSub
	BinaryOpMul
	BinaryOpQuo
	BinaryOpRem

	BinaryOpEq
	BinaryOpNeq
	BinaryOpLt
	BinaryOpLte
	BinaryOpGt
	BinaryOpGte

	BinaryOpAnd
	BinaryOpOr
	BinaryOpXor
	BinaryOpAndNot
	BinaryOpShl
	BinaryOpShr

	BinaryOpLAnd
	BinaryOpLOr

	BinaryOpArrow
	// TODO: div, mod, etc.
)

func (op BinaryOp) String() string {
	switch op {
	case BinaryOpAdd:
		return "+"
	case BinaryOpSub:
		return "-"
	case BinaryOpMul:
		return "*"
	case BinaryOpEq:
		return "=="
	case BinaryOpNeq:
		return "!="
	case BinaryOpLt:
		return "<"
	case BinaryOpLte:
		return "<="
	case BinaryOpGt:
		return ">"
	case BinaryOpGte:
		return ">="
	case BinaryOpAnd:
		return "&"
	case BinaryOpOr:
		return "|"
	case BinaryOpLAnd:
		return "&&"
	case BinaryOpLOr:
		return "||"
	case BinaryOpArrow:
		return "<-"
	default:
		panic("unreachable")
	}
}

type UnaryOp int

const (
	UnaryOpPos UnaryOp = iota
	UnaryOpNeg

	UnaryOpNot

	UnaryOpAddr
	UnaryOpDeref

	UnaryOpArrow

	UnaryOpBitNot
)

func (op UnaryOp) String() string {
	switch op {
	case UnaryOpPos:
		return "+"
	case UnaryOpNeg:
		return "-"
	case UnaryOpNot:
		return "!"
	case UnaryOpBitNot:
		return "^"
	case UnaryOpAddr:
		return "&"
	case UnaryOpDeref:
		return "*"
	case UnaryOpArrow:
		return "<-"
	default:
		panic("unreachable")
	}
}

// ========================

type Expr interface {
	_Expr()
}

type ExprBase struct{}

func (ExprBase) _Expr() {}

type EllipsisExpr struct {
	ExprBase
}

type BinaryExpr struct {
	ExprBase
	Op    BinaryOp
	Left  Expr
	Right Expr
}

type UnaryExpr struct {
	ExprBase
	Op   UnaryOp
	Expr Expr
}

type StarExpr struct {
	ExprBase
	Expr Expr
}

type AddressExpr struct {
	ExprBase
	Expr Expr
}

type ConversionExpr struct {
	ExprBase
	Type Type
	Expr Expr
}

// TODO: MethodExpr

type SelectorExpr struct {
	ExprBase
	Expr Expr
	Sel  Identifier
}

type IndexExpr struct {
	ExprBase
	Expr    Expr
	Indices []Expr
}

type SliceExpr struct {
	ExprBase
	Expr Expr
	Low  Expr
	High Expr
	Max  Expr
}

// TODO: SliceExpr

type TypeAssertionExpr struct {
	ExprBase
	Expr Expr
	Type Type
}

type CallExpr struct {
	ExprBase
	Func Expr
	Args []Expr
	// TODO: Ellipsis
}

type NameExpr struct {
	ExprBase
	Name Identifier
}

type LiteralExpr struct {
	ExprBase
	Literal Literal
}

type FuncLitExpr struct {
	ExprBase
	Signature Signature
	Body      StatementList
}

type CompositeLitExpr struct {
	ExprBase
	Type  Type
	Elems []CompositeLitElem
}

type CompositeLitElem struct {
	Key   Expr
	Value Expr
}

type Literal interface {
	_Literal()
}

type LiteralBase struct{}

func (LiteralBase) _Literal() {}

type LiteralInt struct {
	LiteralBase
	Value string
}

type LiteralBool struct {
	LiteralBase
	Value string
}

type LiteralString struct {
	LiteralBase
	Value string
}

type LiteralFloat struct {
	LiteralBase
	Value string
}

type LiteralImag struct {
	LiteralBase
	Value string
}

type LiteralRune struct {
	LiteralBase
	Value string
}

// for things like make() and new()
type TypeExpr struct {
	ExprBase
	Type Type
}

// ========================

type Statement interface {
	_Statement()
}

type StatementBase struct{}

func (StatementBase) _Statement() {}

type DeclStmt struct {
	StatementBase
	Decl Decl
}

type ReturnStmt struct {
	StatementBase
	Results []Expr
}

type BranchStmt struct {
	StatementBase
	// TODO type
}

type IfStmt struct {
	StatementBase
	Init Statement
	Cond Expr
	Body StatementList
	Else *IfStmt // Cond==True for plain else
}

// TODO: SelectStmt, LabeledStmt, BlockStmt

type GoStmt struct {
	StatementBase
	Call *CallExpr
}

type DeferStmt struct {
	StatementBase
	Call *CallExpr
}

type ExpressionStmt struct {
	StatementBase
	Expr Expr
}

type EmptyStmt struct {
	StatementBase
}

type IncDecStmt struct {
	StatementBase
	Expr Expr
	Inc  bool
}

type SendStmt struct {
	StatementBase
	Chan  Expr
	Value Expr
}

type ReceiveStmt struct {
	StatementBase
	Chan Expr
}

type SelectStmt struct {
	StatementBase
	Cases []SelectCase
}

type SelectCase struct {
	Comm Statement
	Body StatementList
}

type AssignmentStmt struct {
	StatementBase
	LHS []Expr
	RHS []Expr
}

type ShortVarDecl struct {
	StatementBase
	Names []Identifier
	Exprs []Expr
}

type RangeStmt struct {
	StatementBase
	Assign     bool
	Key, Value Expr
	X          Expr
	Body       StatementList
}

type ForStmt struct {
	StatementBase
	Init Statement
	Cond Expr
	Post Statement
	Body StatementList
}

type TypeSwitchStmt struct {
	StatementBase
	Init   Statement
	Assign Statement
	Body   []TypeSwitchCase
}

type TypeSwitchCase struct {
	Types []Type
	Body  StatementList
}

type SwitchStmt struct {
	StatementBase
	Init  Statement
	Tag   Expr // ???
	Cases []SwitchCase
}

type SwitchCase struct {
	Exprs []Expr
	Body  StatementList
}

type BlockStmt struct {
	StatementBase
	Body StatementList
}

type StatementList struct {
	Stmts []Statement
}

// ========================

type Decl interface {
	_Decl()
}

type DeclBase struct{}

func (DeclBase) _Decl() {}

type ImportDecl struct {
	DeclBase
	Path  string
	Alias *Identifier
}

func (d *ImportDecl) EffectiveName() string {
	if d.Alias != nil {
		return d.Alias.Value
	}
	parts := strings.Split(d.Path, "/")
	return parts[len(parts)-1]
}

type ConstDecl struct {
	DeclBase
	Elems []ConstDeclElem
}

type ConstDeclElem struct {
	DeclBase
	Name  Identifier
	Type  Type
	Value Expr
}

type TypeDecl struct {
	DeclBase
	Name       Identifier
	TypeParams TypeParamList
	Type       Type
}

type AliasDecl struct {
	DeclBase
	Name Identifier
	Type Type
}

type VarDecl struct {
	DeclBase
	Name Identifier
	Type Type
	Expr Expr
}

type FunctionDecl struct {
	DeclBase
	Name      Identifier
	Signature Signature
	Body      StatementList
}

type MethodDecl struct {
	DeclBase
	Name      Identifier
	Receiver  FieldDecl
	Signature Signature
	Body      StatementList
}

type TypeParamList struct {
	Params []TypeParamDecl
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

// ========================

type File struct {
	Path  string
	Decls []Decl
}

// ========================

type Relation interface {
	_Relation()
}

type RelationBase struct{}

func (RelationBase) _Relation() {}

type RelationEq struct {
	RelationBase
	Left  Type
	Right Type
}

func (r RelationEq) String() string {
	return fmt.Sprintf("%v =eq %v", r.Left, r.Right)
}

type RelationSubtype struct {
	RelationBase
	Sub   Type
	Super Type
}

func (r RelationSubtype) String() string {
	return fmt.Sprintf("%v <: %v", r.Sub, r.Super)
}

type RelationSatisfies struct {
	RelationBase
	Type       Type
	Constraint *InterfaceType
}

func (r RelationSatisfies) String() string {
	return fmt.Sprintf("%v sat %v", r.Type, r.Constraint)
}

type TypeContext struct {
	Parent    *TypeContext
	Relations []Relation
}

func (c TypeContext) String() string {
	parent := ""
	if c.Parent != nil {
		parent = fmt.Sprintf("%v || ", c.Parent)
	}
	parts := []string{}
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

func (c *TypeContext) AddEq(left, right Type) {
	c.AddRelation(RelationEq{Left: left, Right: right})
}

func (c *TypeContext) Fork() TypeContext {
	return TypeContext{Parent: c, Relations: []Relation{}}

}

// ========================

type VarContext struct {
	Parent *VarContext
	Types  map[Identifier]Type
}

func EmptyVarContext() VarContext {
	return VarContext{Types: map[Identifier]Type{}}
}

func (c VarContext) String() string {
	parts := []string{}
	c.Iter(func(name Identifier, ty Type) {
		parts = append(parts, fmt.Sprintf("%v: %v", name, ty))
	})
	return strings.Join(parts, "\n")
}

func (c *VarContext) Def(name Identifier, ty Type) Type {
	if name != IgnoreIdent {
		c.Types[name] = ty
	}
	return ty
}

func (c *VarContext) DefBuiltinFunction(name string) Type {
	return c.Def(NewIdentifier(name), &BuiltinFunctionType{Name: name})
}

func (c *VarContext) DefType(name Identifier, ty Type) Type {
	return c.Def(name, &TypeOfType{Type: ty})
}

func (c *VarContext) DefNamedType(name Identifier, under Type) Type {
	return c.DefType(name, &NamedType{Name: name, Type: under})
}

func (c *VarContext) DefBuiltinType(name string) Type {
	return c.DefType(NewIdentifier(name), NewBuiltinType(name))
}

func (c *VarContext) Lookup(name Identifier) (Type, bool) {
	ty, ok := c.Types[name]
	if !ok && c.Parent != nil {
		return c.Parent.Lookup(name)
	}
	return ty, ok
}

func (c *VarContext) Fork() VarContext {
	return VarContext{Parent: c, Types: map[Identifier]Type{}}
}

func (c *VarContext) Iter(f func(Identifier, Type)) {
	if c.Parent != nil {
		c.Parent.Iter(f)
	}
	for name, ty := range c.Types {
		f(name, ty)
	}
}

// ========================

func MakeBuiltins() VarContext {
	scope := EmptyVarContext()

	scope.DefBuiltinType("bool")
	scope.Def(NewIdentifier("true"), &UntypedConstantType{Kind: UntypedConstantBool})
	scope.Def(NewIdentifier("false"), &UntypedConstantType{Kind: UntypedConstantBool})

	scope.DefBuiltinType("uint8")
	scope.DefBuiltinType("uint16")
	scope.DefBuiltinType("uint32")
	scope.DefBuiltinType("uint64")

	scope.DefBuiltinType("int8")
	scope.DefBuiltinType("int16")
	scope.DefBuiltinType("int32")
	scope.DefBuiltinType("int64")

	scope.DefBuiltinType("float32")
	scope.DefBuiltinType("float64")

	scope.DefBuiltinType("complex64")
	scope.DefBuiltinType("complex128")

	scope.DefBuiltinType("string")

	scope.DefBuiltinType("int")
	scope.DefBuiltinType("uint")
	scope.DefBuiltinType("uintptr")

	scope.DefType(NewIdentifier("byte"), NewBuiltinType("uint8"))
	scope.DefType(NewIdentifier("rune"), NewBuiltinType("int32"))

	scope.DefType(NewIdentifier("any"), EmptyInterface())

	// TODO comparable constraint

	scope.Def(NewIdentifier("iota"), &UntypedConstantType{Kind: UntypedConstantInt})

	scope.Def(NewIdentifier("nil"), &NilType{})

	scope.DefBuiltinFunction("append")
	scope.DefBuiltinFunction("copy")
	scope.DefBuiltinFunction("delete")
	scope.DefBuiltinFunction("len")
	scope.DefBuiltinFunction("cap")
	scope.DefBuiltinFunction("make")
	scope.DefBuiltinFunction("max")
	scope.DefBuiltinFunction("min")
	scope.DefBuiltinFunction("new")
	scope.DefBuiltinFunction("complex")
	scope.DefBuiltinFunction("real")
	scope.DefBuiltinFunction("imag")
	scope.DefBuiltinFunction("clear")
	scope.DefBuiltinFunction("close")
	scope.DefBuiltinFunction("panic")
	scope.DefBuiltinFunction("recover")
	scope.DefBuiltinFunction("print")
	scope.DefBuiltinFunction("println")

	scope.DefNamedType(NewIdentifier("error"), ParseType("interface{Error() string}"))

	return scope
}

type Checker struct {
	Fresh *int

	TyCtx    TypeContext
	VarCtx   VarContext
	Builtins VarContext

	// this seems unprincipled
	CurFn *Signature
	CurTy *TypeDecl
}

func NewChecker() *Checker {
	return &Checker{
		Fresh:    Ptr(0),
		TyCtx:    TypeContext{},
		VarCtx:   EmptyVarContext(),
		Builtins: MakeBuiltins(),
	}
}

// ========================

func (c *Checker) FreshTypeName() Identifier {
	*c.Fresh = *c.Fresh + 1
	return Identifier{Value: fmt.Sprintf("@T%d", *c.Fresh)}
}

func (c *Checker) Lookup(name Identifier) Type {
	ty, ok := c.VarCtx.Lookup(name)
	if ok {
		return ty
	}
	ty, ok = c.Builtins.Lookup(name)
	if ok {
		return ty
	}
	panic(fmt.Errorf("undefined: %v", name))
}

func (c *Checker) DefineValue(name Identifier, ty Type) {
	c.VarCtx.Def(name, ty)
}

func (c *Checker) DefineType(name Identifier, ty Type) {
	c.VarCtx.Def(name, &TypeOfType{Type: ty})
}

func (c *Checker) BuiltinValue(name string) Type {
	ty, ok := c.Builtins.Lookup(NewIdentifier(name))
	if !ok {
		panic(fmt.Errorf("undefined builtin: %v", name))
	}
	return ty
}

func (c *Checker) BuiltinType(name string) Type {
	ty, ok := c.Builtins.Lookup(NewIdentifier(name))
	if !ok {
		panic(fmt.Errorf("undefined builtin: %v", name))
	}
	realTy, ok := ty.(*TypeOfType)
	if !ok {
		panic(fmt.Errorf("not a builtin type: %v", ty))
	}
	return realTy.Type
}

// ========================

func (c *Checker) ResolveValue(ty Type) Type {
	switch ty := ty.(type) {
	case *TypeName:
		return c.Lookup(ty.Name)
	case *QualIdentifier:
		// TODO hacky
		if ty.Package == "" {
			return c.Lookup(ty.Name)
		}
		pkg := c.Lookup(NewIdentifier(ty.Package))
		imp, ok := pkg.(*ImportType)
		if !ok {
			panic("not an import")
		}
		retTy, ok := imp.Scope.Lookup(ty.Name)
		if !ok {
			panic(fmt.Sprintf("definition %v not found in package %v", ty.Name, ty.Package))
		}
		return retTy
	default:
		return ty
	}
}

func (c *Checker) ResolveType(ty Type) Type {
	var valueTy Type
	switch ty := ty.(type) {
	case *TypeName:
		valueTy = c.ResolveValue(ty)
	case *QualIdentifier:
		valueTy = c.ResolveValue(ty)
	default:
		return ty
	}

	switch valueTy := valueTy.(type) {
	case *TypeOfType:
		return valueTy.Type
	default:
		panic(fmt.Errorf("not a type: %v", ty))
	}
}

func (c *Checker) CheckAssignableTo(src, dst Type) {
	c.TyCtx.AddRelation(RelationSubtype{Sub: src, Super: dst})
}

// ========================

func (c *Checker) BeginScope() *Checker {
	return &Checker{
		Fresh:    c.Fresh,
		TyCtx:    c.TyCtx.Fork(),
		VarCtx:   c.VarCtx.Fork(),
		Builtins: c.Builtins,
		CurFn:    c.CurFn,
		CurTy:    c.CurTy,
	}
}

func (c *Checker) BeginTypeScope(decl *TypeDecl) *Checker {
	return &Checker{
		Fresh:    c.Fresh,
		TyCtx:    c.TyCtx.Fork(),
		VarCtx:   c.VarCtx.Fork(),
		Builtins: c.Builtins,
		CurFn:    c.CurFn,
		CurTy:    decl,
	}
}

func (c *Checker) BeginFunctionScope(fn *Signature) *Checker {
	return &Checker{
		Fresh:    c.Fresh,
		TyCtx:    c.TyCtx.Fork(),
		VarCtx:   c.VarCtx.Fork(),
		Builtins: c.Builtins,
		CurFn:    fn,
		CurTy:    c.CurTy,
	}
}

func (c *Checker) AssertInFunctionScope() *Signature {
	if c.CurFn == nil {
		panic("not in function scope")
	}
	return c.CurFn
}

// ========================

func (c *Checker) LoadFile(file File) {
	fmt.Printf("=== LoadFile(%v) ===\n", file.Path)
	for _, decl := range file.Decls {
		c.DefineDecl(decl)
	}
}

func (c *Checker) CheckFile(file File) {
	fmt.Printf("=== CheckFile(%v) ===\n", file.Path)
	for _, decl := range file.Decls {
		c.CheckDecl(decl)
	}
}

// ========================

func (c *Checker) DefineDecl(decl Decl) {
	switch decl := decl.(type) {
	case *ImportDecl:
		c.DefineImportDecl(decl)
	case *ConstDecl:
		c.DefineConstDecl(decl)
	case *TypeDecl:
		c.DefineTypeDecl(decl)
	case *AliasDecl:
		c.DefineAliasDecl(decl)
	case *VarDecl:
		c.DefineVarDecl(decl)
	case *FunctionDecl:
		c.DefineFunctionDecl(decl)
	case *MethodDecl:
		c.DefineMethodDecl(decl)
	default:
		spew.Dump(decl)
		panic("unreachable")
	}
}

func (c *Checker) DefineImportDecl(decl *ImportDecl) {
	fmt.Printf("=== DefineImportDecl(%v) ===\n", decl.Path)

	var scope VarContext

	switch decl.Path {
	case "unsafe":
		scope = c.ReadUnsafePackage()
	default:
		scope = c.ReadPackage(decl)
	}

	c.DefineValue(NewIdentifier(decl.EffectiveName()), &ImportType{Decl: *decl, Scope: scope})

	fmt.Printf("=== done DefineImportDecl(%v) ===\n", decl.Path)
}

func (c *Checker) ReadUnsafePackage() VarContext {
	scope := EmptyVarContext()
	scope.DefType(NewIdentifier("Pointer"), NewBuiltinType("Pointer"))
	scope.Def(NewIdentifier("Sizeof"), ParseType("func(interface{}) uintptr"))
	scope.Def(NewIdentifier("Offsetof"), ParseType("func(interface{}) uintptr"))
	scope.Def(NewIdentifier("Alignof"), ParseType("func(interface{}) uintptr"))
	scope.Def(NewIdentifier("Add"), ParseType("func(Pointer, int) Pointer"))
	scope.Def(NewIdentifier("String"), ParseType("func(*byte, int) string"))
	scope.Def(NewIdentifier("StringData"), ParseType("func(string) *byte"))
	return scope
}

var packageCache = map[string]VarContext{} // TODO are paths relative or module-based?

func (c *Checker) ReadPackage(decl *ImportDecl) VarContext {
	if cached, ok := packageCache[decl.Path]; ok {
		fmt.Printf("using cached %v\n", decl.Path)
		return cached
	}

	out, err := exec.Command("go", "env", "GOROOT").Output()
	if err != nil {
		panic(fmt.Errorf("failed to get GOROOT: %w", err))
	}

	goroot := strings.TrimSpace(string(out))
	path := filepath.Join(goroot, "src", decl.Path)

	fmt.Printf("parsing %v\n", path)

	packages, err := parser.ParseDir(token.NewFileSet(), path, nil, parser.ParseComments)
	if err != nil {
		panic(fmt.Errorf("failed to parse GOROOT: %w", err))
	}

	child := NewChecker()

	files := []File{}
	for _, pkg := range packages {
		fmt.Printf("package %v\n", pkg.Name)
		if strings.HasSuffix(pkg.Name, "_test") {
			// TODO handle "." import
			fmt.Printf("skipping test package\n")
			continue
		}
		for filename, f := range pkg.Files {
			if strings.HasSuffix(filename, "_test.go") {
				fmt.Printf("skipping test file %v\n", filename)
				continue
			}
			files = append(files, ReadAST(filename, f))
		}
	}

	packageCache[decl.Path] = child.VarCtx

	for _, file := range files {
		child.LoadFile(file)
	}
	for _, file := range files {
		child.CheckFile(file)
	}

	return child.VarCtx
}

func (c *Checker) DefineConstDecl(decl *ConstDecl) {
	var carryTy Type
	for _, elem := range decl.Elems {
		var elemTy Type
		if elem.Type == nil {
			if elem.Value == nil {
				if carryTy == nil {
					panic("mising init expr for constant")
				}
				elemTy = carryTy
			} else {
				elemTy = c.Synth(elem.Value)
				if ContainsIota(elem.Value) {
					carryTy = elemTy
				}
			}
		} else {
			carryTy = nil
			elemTy = elem.Type
			if elem.Value != nil {
				c.CheckAssignableTo(c.Synth(elem.Value), elemTy)
				if ContainsIota(elem.Value) {
					carryTy = elemTy
				}
			} else {
				panic("mising init expr for constant")
			}
		}
		if elemTy == nil {
			panic("OOPS")
		}
		c.DefineValue(elem.Name, elemTy)
	}
}

func ContainsIota(expr Expr) bool {
	switch expr := expr.(type) {
	case *NameExpr:
		return expr.Name.Value == "iota"
	case *BinaryExpr:
		return ContainsIota(expr.Left) || ContainsIota(expr.Right)
	case *UnaryExpr:
		return ContainsIota(expr.Expr)
	case *StarExpr:
		return ContainsIota(expr.Expr)
	case *AddressExpr:
		return ContainsIota(expr.Expr)
	case *ConversionExpr:
		return ContainsIota(expr.Expr)
	case *SelectorExpr:
		return ContainsIota(expr.Expr)
	case *IndexExpr:
		return ContainsIota(expr.Expr)
	case *SliceExpr:
		return ContainsIota(expr.Expr) || ContainsIota(expr.Low) || ContainsIota(expr.High) || ContainsIota(expr.Max)
	case *TypeAssertionExpr:
		return ContainsIota(expr.Expr)
	case *CallExpr:
		for _, arg := range expr.Args {
			if ContainsIota(arg) {
				return true
			}
		}
		return false
	case *LiteralExpr:
		return false
	case *FuncLitExpr:
		return false
	case *CompositeLitExpr:
		return false
	case *TypeExpr:
		return false
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func (c *Checker) DefineTypeDecl(decl *TypeDecl) {
	var ty Type
	if len(decl.TypeParams.Params) > 0 {
		ty = &NamedType{Name: decl.Name, Type: &GenericType{TypeParams: decl.TypeParams, Type: decl.Type}}
	} else {
		ty = &NamedType{Name: decl.Name, Type: decl.Type}
	}
	c.DefineType(decl.Name, ty)
}

func (c *Checker) DefineAliasDecl(decl *AliasDecl) {
	c.DefineType(decl.Name, decl.Type)
}

func (c *Checker) DefineVarDecl(decl *VarDecl) {
	var ty Type
	if decl.Type == nil {
		ty = &LazyType{Expr: decl.Expr}
	} else {
		ty = decl.Type
	}
	c.DefineValue(decl.Name, ty)
}

func (c *Checker) DefineFunctionDecl(decl *FunctionDecl) {
	c.DefineValue(decl.Name, &FunctionType{Signature: decl.Signature})
}

func (c *Checker) DefineMethodDecl(decl *MethodDecl) {
	pointerReceiver := false

	receiverTy := decl.Receiver.Type
	if pointerTy, ok := receiverTy.(*PointerType); ok {
		receiverTy = pointerTy.BaseType
		pointerReceiver = true
	}

	var methodHolder Identifier

	switch receiverTy := receiverTy.(type) {
	case *TypeName:
		methodHolder = receiverTy.Name
	case *TypeApplication:
		if receiverTy.ID.Package != "" {
			panic("cannot have package-qualified receiver type")
		}
		methodHolder = receiverTy.ID.Name
	default:
		spew.Dump(receiverTy)
		panic("TODO")
	}

	methodTy := &MethodType{
		PointerReceiver: pointerReceiver,
		Type:            &FunctionType{Signature: decl.Signature},
	}

	methodName := NewIdentifier(fmt.Sprintf("%s.%s", methodHolder.Value, decl.Name.Value))

	c.DefineValue(methodName, methodTy)
}

// ========================

func (c *Checker) CheckDecl(decl Decl) {
	switch decl := decl.(type) {
	case *ImportDecl:
		c.CheckImportDecl(decl)
	case *ConstDecl:
		c.CheckConstDecl(decl)
	case *TypeDecl:
		c.CheckTypeDecl(decl)
	case *AliasDecl:
		c.CheckAliasDecl(decl)
	case *VarDecl:
		c.CheckVarDecl(decl)
	case *FunctionDecl:
		c.CheckFunctionDecl(decl)
	case *MethodDecl:
		c.CheckMethodDecl(decl)
	default:
		spew.Dump(decl)
		panic("unreachable")
	}
}

func (c *Checker) CheckImportDecl(decl *ImportDecl) {
	// nothing to do
}

func (c *Checker) CheckConstDecl(decl *ConstDecl) {
	// TODO: check constant expressions?
}

func (c *Checker) CheckTypeDecl(decl *TypeDecl) {
	fmt.Printf("=== CheckTypeDecl(%v) ===\n", decl.Name)

	scope := c.BeginTypeScope(decl)

	for _, tyParam := range decl.TypeParams.Params {
		scope.DefineType(tyParam.Name, &TypeParam{Name: tyParam.Name, Bound: tyParam.Constraint})
	}

	scope.CheckTypeDeclType(decl.Type)

	subst := scope.Verify()

	scope.CheckSubst(decl.TypeParams, subst)
}

func (c *Checker) CheckTypeDeclType(ty Type) {
	switch ty := c.ResolveType(ty).(type) {
	case *TypeBuiltin:
		// nothing to do
	case *TypeParam:
		// nothing to do
	case *TypeApplication:
		c.TypeApplicationFunc(ty, func(tyParam TypeParamDecl, tyArg Type) {
			c.CheckTypeDeclType(tyArg)
		})
	case *StructType:
		for _, field := range ty.Fields {
			c.CheckTypeDeclType(field.Type)
		}
	case *InterfaceType:
		for _, m := range ty.Methods {
			c.CheckTypeDeclType(m.Type)
		}
		for _, ctr := range ty.Constraints {
			for _, term := range ctr.TypeElem.Union {
				c.CheckTypeDeclType(term.Type)
			}
		}
	case *FunctionType:
		c.CheckTypeDeclSignature(ty.Signature)
	case *SliceType:
		c.CheckTypeDeclType(ty.ElemType)
	case *NamedType:
		c.CheckTypeDeclType(ty.Type)
	case *PointerType:
		c.CheckTypeDeclType(ty.BaseType)
	case *ArrayType:
		c.CheckTypeDeclType(ty.ElemType)
	case *MapType:
		c.CheckTypeDeclType(ty.KeyType)
		c.CheckTypeDeclType(ty.ElemType)
	default:
		spew.Dump(ty)
		panic("unreachable")
	}
}

func (c *Checker) CheckTypeDeclSignature(sig Signature) {
	if len(sig.TypeParams.Params) > 0 {
		panic("function type with type parameters")
	}
	for _, param := range sig.Params.Params {
		c.CheckTypeDeclType(param.Type)
	}
	for _, result := range sig.Results.Params {
		c.CheckTypeDeclType(result.Type)
	}
}

func (c *Checker) CheckAliasDecl(decl *AliasDecl) {
	// nothing to do
}

func (c *Checker) CheckVarDecl(decl *VarDecl) {
	if decl.Type != nil && decl.Expr != nil {
		c.CheckAssignableTo(c.Synth(decl.Expr), decl.Type)
	}
}

func (c *Checker) CheckMethodDecl(decl *MethodDecl) {
	fmt.Printf("=== CheckMethodDecl(%v) ===\n", decl.Name)

	scope := c.BeginFunctionScope(&decl.Signature)

	var receiverTy Type = c.ResolveType(decl.Receiver.Type)
	var pointerReceiver bool

	if pointerTy, ok := receiverTy.(*PointerType); ok {
		pointerReceiver = true
		receiverTy = c.ResolveType(pointerTy.BaseType)
	}

	var methodHolder *NamedType

	switch ty := receiverTy.(type) {
	case *NamedType:
		methodHolder = ty
	case *TypeApplication:
		named, ok := c.ResolveType(&ty.ID).(*NamedType)
		if !ok {
			panic("method on non-named type")
		}
		gen, ok := named.Type.(*GenericType)
		if !ok {
			panic("not a generic type")
		}
		if len(gen.TypeParams.Params) != len(ty.Args) {
			panic("wrong number of type arguments")
		}
		for _, tyParam := range gen.TypeParams.Params {
			scope.DefineType(tyParam.Name, &TypeParam{Name: tyParam.Name, Bound: tyParam.Constraint})
		}
		methodHolder = named
	default:
		panic("method on non-named type")
	}

	for _, tyParam := range decl.Signature.TypeParams.Params {
		scope.DefineType(tyParam.Name, &TypeParam{Name: tyParam.Name, Bound: tyParam.Constraint})
	}

	scope.DefineValue(decl.Receiver.Name, decl.Receiver.Type)

	for _, param := range decl.Signature.Params.Params {
		scope.DefineValue(param.Name, param.Type)
	}

	for _, result := range decl.Signature.Results.Params {
		scope.DefineValue(result.Name, result.Type)
	}

	scope.CheckStatementList(decl.Body)

	subst := scope.Verify()
	scope.CheckSubst(decl.Signature.TypeParams, subst)

	_ = methodHolder
	_ = pointerReceiver
}

func (c *Checker) CheckFunctionDecl(decl *FunctionDecl) {
	fmt.Printf("=== CheckFunctionDecl(%v) ===\n", decl.Name)

	scope := c.BeginFunctionScope(&decl.Signature)

	for _, tyParam := range decl.Signature.TypeParams.Params {
		scope.DefineType(tyParam.Name, &TypeParam{Name: tyParam.Name, Bound: tyParam.Constraint})
	}

	for _, param := range decl.Signature.Params.Params {
		if param.Variadic {
			scope.DefineValue(param.Name, &SliceType{ElemType: param.Type})
		} else {
			scope.DefineValue(param.Name, param.Type)
		}
	}

	for _, result := range decl.Signature.Results.Params {
		scope.DefineValue(result.Name, result.Type)
	}

	scope.CheckStatementList(decl.Body)

	subst := scope.Verify()
	scope.CheckSubst(decl.Signature.TypeParams, subst)
}

func (c *Checker) CheckStatementList(list StatementList) {
	for _, stmt := range list.Stmts {
		c.CheckStatement(stmt)
	}
}

func (c *Checker) CheckStatement(stmt Statement) {
	switch stmt := stmt.(type) {
	case *DeclStmt:
		c.DefineDecl(stmt.Decl)
		c.CheckDecl(stmt.Decl)
	case *ExpressionStmt:
		c.Synth(stmt.Expr) // ???
	case *EmptyStmt:
		// do nothing
	case *ReturnStmt:
		c.CheckReturnStmt(stmt)
	case *IfStmt:
		c.CheckIfStmt(stmt)
	case *ShortVarDecl:
		c.CheckShortVarDecl(stmt)
	case *RangeStmt:
		c.CheckRangeStmt(stmt)
	case *IncDecStmt:
		c.CheckIncDecStmt(stmt)
	case *AssignmentStmt:
		c.CheckAssignmentStmt(stmt)
	case *BranchStmt:
		// nothing?
	case *ForStmt:
		c.CheckForStmt(stmt)
	case *SwitchStmt:
		c.CheckSwitchStmt(stmt)
	default:
		spew.Dump(stmt)
		panic("unreachable")
	}
}

func (c *Checker) CheckShortVarDecl(stmt *ShortVarDecl) {
	if len(stmt.Names) == len(stmt.Exprs) {
		for i, name := range stmt.Names {
			ty := c.Synth(stmt.Exprs[i])
			c.DefineValue(name, ty)
		}
	} else if len(stmt.Names) > 1 && len(stmt.Exprs) == 1 {
		ty := c.Synth(stmt.Exprs[0])
		switch ty := c.Under(ty).(type) {
		case *TupleType:
			if len(stmt.Names) != len(ty.Elems) {
				panic("wrong number of return in tuple expansion")
			}
			for i, name := range stmt.Names {
				c.DefineValue(name, ty.Elems[i])
			}
		default:
			panic("non-tuple type")
		}
	} else {
		panic("wrong number of expressions")
	}
}

func (c *Checker) CheckAssignmentStmt(stmt *AssignmentStmt) {
	if len(stmt.LHS) != len(stmt.RHS) {
		// TODO tuple assignment
		panic("wrong number of expressions")
	}
	for i, lhs := range stmt.LHS {
		ty := c.Synth(stmt.RHS[i])
		c.CheckAssignableTo(ty, c.Synth(lhs))
	}
}

func (c *Checker) CheckReturnStmt(stmt *ReturnStmt) {
	fn := c.AssertInFunctionScope()
	if len(stmt.Results) == len(fn.Results.Params) {
		for i, result := range stmt.Results {
			ty := c.Synth(result)
			c.CheckAssignableTo(ty, fn.Results.Params[i].Type)
		}
	} else if len(stmt.Results) == 1 && len(fn.Results.Params) > 0 {
		ty := c.Synth(stmt.Results[0])
		switch ty := c.Under(ty).(type) {
		case *TupleType:
			if len(fn.Results.Params) != len(ty.Elems) {
				panic("wrong number of return in tuple expansion")
			}
			for i, param := range fn.Results.Params {
				c.CheckAssignableTo(ty.Elems[i], param.Type)
			}
		default:
			panic("non-tuple type")
		}
	} else if len(stmt.Results) == 0 && fn.HasNamedResults() {
		return
	} else {
		panic("wrong number of expressions in return")
	}
}

func (c *Checker) CheckIfStmt(stmt *IfStmt) {
	if stmt.Init != nil {
		c.CheckStatement(stmt.Init)
	}
	if stmt.Cond != nil {
		// TODO only allowed for else
		c.CheckExpr(stmt.Cond, c.BuiltinType("bool"))
	}
	c.CheckStatementList(stmt.Body)
	if stmt.Else != nil {
		c.CheckStatement(stmt.Else)
	}
}

func (c *Checker) CheckIncDecStmt(stmt *IncDecStmt) {
	exprTy := c.Synth(stmt.Expr)
	if !c.IsNumeric(exprTy) {
		panic("non-numeric type")
	}
	// TODO emit relation instead of greedy check
}

func (c *Checker) CheckRangeStmt(stmt *RangeStmt) {
	scope := c.BeginScope()

	targetTy := scope.Synth(stmt.X)

	switch targetTy := scope.Under(targetTy).(type) {
	case *SliceType:
		if stmt.Key != nil {
			if stmt.Assign {
				scope.DefineValue(stmt.Key.(*NameExpr).Name, scope.BuiltinType("int"))
			} else {
				scope.CheckAssignableTo(c.BuiltinType("int"), scope.Synth(stmt.Key))
			}
		}
		if stmt.Value != nil {
			if stmt.Assign {
				scope.DefineValue(stmt.Value.(*NameExpr).Name, targetTy.ElemType)
			} else {
				scope.CheckAssignableTo(targetTy.ElemType, scope.Synth(stmt.Value))
			}
		}
	case *MapType:
		if stmt.Key != nil {
			if stmt.Assign {
				scope.DefineValue(stmt.Key.(*NameExpr).Name, targetTy.KeyType)
			} else {
				scope.CheckAssignableTo(targetTy.KeyType, scope.Synth(stmt.Key))
			}
		}
		if stmt.Value != nil {
			if stmt.Assign {
				scope.DefineValue(stmt.Value.(*NameExpr).Name, targetTy.ElemType)
			} else {
				scope.CheckAssignableTo(targetTy.ElemType, scope.Synth(stmt.Value))
			}
		}
	default:
		spew.Dump(targetTy)
		panic(fmt.Sprintf("cannot range over %v", targetTy))
	}

	scope.CheckStatementList(stmt.Body)
}

func (c *Checker) CheckForStmt(stmt *ForStmt) {
	scope := c.BeginScope()
	if stmt.Init != nil {
		scope.CheckStatement(stmt.Init)
	}
	if stmt.Cond != nil {
		scope.CheckExpr(stmt.Cond, c.BuiltinType("bool"))
	}
	if stmt.Post != nil {
		scope.CheckStatement(stmt.Post)
	}
	scope.CheckStatementList(stmt.Body)
}

func (c *Checker) CheckSwitchStmt(stmt *SwitchStmt) {
	scope := c.BeginScope()
	if stmt.Init != nil {
		scope.CheckStatement(stmt.Init)
	}
	if stmt.Tag != nil {
		spew.Dump(stmt)
		panic("PANIC what is a tag?")
	}
	for _, caseStmt := range stmt.Cases {
		scope.CheckSwitchCaseStmt(caseStmt)
	}
}

func (c *Checker) CheckSwitchCaseStmt(stmt SwitchCase) {
	for _, expr := range stmt.Exprs {
		c.CheckExpr(expr, c.BuiltinType("bool"))
	}
	c.CheckStatementList(stmt.Body)
}

// ========================

type TypeSet struct {
	Methods  []MethodElem
	Types    []Type
	Universe bool
}

func (c *Checker) Combine(lhs, rhs TypeSet) TypeSet {
	result := TypeSet{
		Methods:  []MethodElem{},
		Types:    []Type{},
		Universe: lhs.Universe && rhs.Universe,
	}

	// combine
	copy(result.Methods, lhs.Methods)

	for _, m := range rhs.Methods {
		for _, n := range lhs.Methods {
			if m.Name == n.Name {
				if !c.Identical(m.Type, n.Type) {
					panic("method clash")
				}
				continue
			}
		}
		result.Methods = append(result.Methods, m)
	}

	if lhs.Universe && rhs.Universe {
		if len(lhs.Types) != 0 {
			panic("weird")
		}
		if len(rhs.Types) != 0 {
			panic("weird")
		}
	} else if lhs.Universe && !rhs.Universe {
		result.Types = append(result.Types, rhs.Types...)
	} else if !lhs.Universe && rhs.Universe {
		result.Types = append(result.Types, lhs.Types...)
	} else {
		// intersect
		for _, t := range rhs.Types {
			for _, u := range lhs.Types {
				if c.Identical(t, u) {
					result.Types = append(result.Types, t)
				}
			}
		}
	}

	return result
}

func (c *Checker) CheckSubst(tyParams TypeParamList, subst Subst) {
	for _, tyParam := range tyParams.Params {
		tySub, ok := subst[tyParam.Name]
		if !ok {
			continue // TODO: is this ok?
		}
		if single, ok := IsSingleTypeUnion(tyParam.Constraint); ok {
			if c.Identical(tySub, c.ResolveType(single)) {
				continue
			}
		}
		panic(fmt.Sprintf("type param %v with constraint %v cannot be %v", tyParam.Name, tyParam.Constraint, tySub))
	}
}

// ========================

var NumericTypes = [...]string{"int", "int8", "int16", "int32", "int64", "uint", "uint8", "uint16", "uint32", "uint64", "float32", "float64"}

func (c *Checker) IsNumeric(ty Type) bool {
	switch ty := c.Under(ty).(type) {
	case *TypeBuiltin:
		return slices.Contains(NumericTypes[:], ty.Name.Value)
	default:
		return false
	}
}

// ========================

func (c *Checker) CheckExpr(expr Expr, ty Type) {
	switch expr := expr.(type) {
	case *BinaryExpr:
		c.CheckBinaryExpr(expr, ty)
	case *UnaryExpr:
		c.CheckUnaryExpr(expr, ty)
	case *ConversionExpr:
		panic("TODO")
	case *SelectorExpr:
		c.CheckSelectorExpr(expr, ty)
	case *IndexExpr:
		c.CheckIndexExpr(expr, ty)
	case *TypeAssertionExpr:
		panic("TODO")
	case *CallExpr:
		c.CheckCallExpr(expr, ty)
	case *NameExpr:
		c.CheckNameExpr(expr, ty)
	case *LiteralExpr:
		c.CheckLiteralExpr(expr, ty)
	case *CompositeLitExpr:
		c.CheckCompositeLitExpr(expr, ty)
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func (c *Checker) CheckBinaryExpr(expr *BinaryExpr, ty Type) {
	c.CheckAssignableTo(c.Synth(expr), ty)
}

func (c *Checker) CheckUnaryExpr(expr *UnaryExpr, ty Type) {
	exprTy := c.Synth(expr.Expr)
	switch expr.Op {
	case UnaryOpNot:
		c.CheckExpr(expr.Expr, c.BuiltinType("bool"))
	case UnaryOpAddr:
		c.CheckAssignableTo(&PointerType{BaseType: exprTy}, ty)
	default:
		spew.Dump(expr, ty)
		panic("unreachable")
	}
}

func (c *Checker) CheckSelectorExpr(expr *SelectorExpr, ty Type) {
	c.CheckAssignableTo(c.Synth(expr), ty)
}

func (c *Checker) CheckIndexExpr(expr *IndexExpr, ty Type) {
	switch exprTy := c.Synth(expr.Expr).(type) {
	case *SliceType:
		if len(expr.Indices) != 1 {
			panic("indexing a slice with multiple indices")
		}
		c.CheckExpr(expr.Indices[0], c.BuiltinType("int"))
		c.TyCtx.AddEq(ty, exprTy.ElemType)
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func (c *Checker) CheckCallExpr(expr *CallExpr, ty Type) {
	callTy := c.Synth(expr)
	c.CheckAssignableTo(callTy, ty)
}

func (c *Checker) CheckNameExpr(expr *NameExpr, ty Type) {
	c.TyCtx.AddEq(c.Lookup(expr.Name), ty)
}

func (c *Checker) CheckLiteralExpr(expr *LiteralExpr, ty Type) {
	exprTy := c.Synth(expr)
	c.CheckAssignableTo(exprTy, ty)
}

func (c *Checker) CheckCompositeLitExpr(expr *CompositeLitExpr, ty Type) {
	if expr.Type == nil {
		c.MakeCompositeLit(expr, ty)
	} else {
		exprTy := c.Synth(expr)
		c.CheckAssignableTo(exprTy, ty)
	}
}

// ========================

func (c *Checker) Synth(expr Expr) Type {
	switch expr := expr.(type) {
	case *BinaryExpr:
		return c.SynthBinaryExpr(expr)
	case *UnaryExpr:
		return c.SynthUnaryExpr(expr)
	case *ConversionExpr:
		return c.SynthConversionExpr(expr)
	case *SelectorExpr:
		return c.SynthSelectorExpr(expr)
	case *IndexExpr:
		return c.SynthIndexExpr(expr)
	case *TypeAssertionExpr:
		panic("TODO")
	case *CallExpr:
		return c.SynthCallExpr(expr)
	case *NameExpr:
		return c.SynthNameExpr(expr)
	case *LiteralExpr:
		return c.SynthLiteralExpr(expr)
	case *TypeExpr:
		return &TypeOfType{Type: c.ResolveType(expr.Type)}
	case *CompositeLitExpr:
		return c.SynthCompositeLitExpr(expr)
	case *SliceExpr:
		return c.SynthSliceExpr(expr)
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func (c *Checker) SynthBinaryExpr(expr *BinaryExpr) Type {
	switch expr.Op {
	case BinaryOpEq, BinaryOpNeq, BinaryOpLt, BinaryOpLte, BinaryOpGt, BinaryOpGte:
		// TODO check comparable
		leftTy := c.Synth(expr.Left)
		rightTy := c.Synth(expr.Right)
		c.TyCtx.AddEq(leftTy, rightTy)
		return c.BuiltinType("bool")
	case BinaryOpAdd, BinaryOpSub, BinaryOpMul, BinaryOpQuo, BinaryOpRem:
		// TODO check numeric?
		leftTy := c.Synth(expr.Left)
		rightTy := c.Synth(expr.Right)
		c.TyCtx.AddEq(leftTy, rightTy)
		return leftTy
	case BinaryOpLAnd, BinaryOpLOr:
		c.CheckExpr(expr.Left, c.BuiltinType("bool"))
		c.CheckExpr(expr.Right, c.BuiltinType("bool"))
		return c.BuiltinType("bool")
	case BinaryOpAnd, BinaryOpOr, BinaryOpXor, BinaryOpAndNot, BinaryOpShl, BinaryOpShr:
		// TODO check numeric?
		leftTy := c.Synth(expr.Left)
		rightTy := c.Synth(expr.Right)
		c.TyCtx.AddEq(leftTy, rightTy)
		return leftTy
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func (c *Checker) SynthUnaryExpr(expr *UnaryExpr) Type {
	ty := c.Synth(expr.Expr)
	switch expr.Op {
	case UnaryOpPos, UnaryOpNeg:
		// TODO check numeric?
		return ty
	case UnaryOpNot:
		c.CheckExpr(expr.Expr, c.BuiltinType("bool"))
		return c.BuiltinType("bool")
	case UnaryOpAddr:
		return &PointerType{BaseType: ty}
	case UnaryOpDeref:
		switch ty := ty.(type) {
		case *PointerType:
			return ty.BaseType
		case *TypeOfType:
			return &TypeOfType{Type: &PointerType{BaseType: ty.Type}}
		default:
			spew.Dump(expr)
			panic(fmt.Sprintf("cannot dereference %v of type %v", expr, ty))
		}
	case UnaryOpBitNot:
		// TODO check numeric?
		return ty
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func (c *Checker) SynthSelectorExpr(expr *SelectorExpr) Type {
	return c.DoSelect(c.Synth(expr.Expr), expr.Sel)
}

func (c *Checker) DoSelect(exprTy Type, sel Identifier) Type {
	switch ty := c.ResolveValue(exprTy).(type) {
	case *ImportType:
		defTy, ok := ty.Scope.Lookup(sel)
		if !ok {
			panic(fmt.Sprintf("import %v has no definition %v", exprTy, sel))
		}
		return defTy
	}

	checkTy := c.ResolveType(exprTy)

	switch ty := checkTy.(type) {
	case *PointerType:
		checkTy = c.ResolveType(ty.BaseType)
	}

	switch ty := checkTy.(type) {
	case *NamedType:
		for _, m := range c.NamedTypeMethods(ty) {
			if m.Name == sel {
				return m.Type
			}
		}
		checkTy = ty.Type
	case *TypeParam:
		if ty.Bound != nil {
			set := c.InterfaceTypeSet(ty.Bound)
			for _, m := range set.Methods {
				if m.Name == sel {
					return m.Type
				}
			}
			if len(set.Types) == 1 {
				return c.DoSelect(c.ResolveType(set.Types[0]), sel)
			}
		}
	}

	switch ty := c.Under(checkTy).(type) {
	case *StructType:
		for _, field := range ty.Fields {
			if field.Name == sel {
				return field.Type
			}
		}

	case *InterfaceType:
		for _, m := range ty.Methods {
			if m.Name == sel {
				return m.Type
			}
		}
	}

	spew.Dump(exprTy)
	panic(fmt.Sprintf("type %v has no field or method %v", exprTy, sel))
}

func (c *Checker) SynthIndexExpr(expr *IndexExpr) Type {
	switch exprTy := c.Synth(expr.Expr).(type) {
	case *SliceType:
		if len(expr.Indices) != 1 {
			panic("indexing a slice with multiple indices")
		}
		c.CheckExpr(expr.Indices[0], c.BuiltinType("int"))
		return exprTy.ElemType
	case *MapType:
		if len(expr.Indices) != 1 {
			panic("indexing a map with multiple indices")
		}
		c.CheckExpr(expr.Indices[0], exprTy.KeyType)
		return exprTy.ElemType
	case *FunctionType:
		panic("unexpected function type (should be handled by CallExpr)")
	case *ArrayType:
		if len(expr.Indices) != 1 {
			panic("indexing an array with multiple indices")
		}
		c.CheckExpr(expr.Indices[0], c.BuiltinType("int"))
		return exprTy.ElemType
	default:
		spew.Dump(reflect.TypeOf(exprTy))
		panic("unreachable")
	}
}

func (c *Checker) SynthSliceExpr(expr *SliceExpr) Type {
	switch exprTy := c.Synth(expr.Expr).(type) {
	case *SliceType:
		if expr.Low != nil {
			c.CheckExpr(expr.Low, c.BuiltinType("int"))
		}
		if expr.High != nil {
			c.CheckExpr(expr.High, c.BuiltinType("int"))
		}
		if expr.Max != nil {
			c.CheckExpr(expr.Max, c.BuiltinType("int"))
		}
		return exprTy
	default:
		spew.Dump(exprTy)
		panic("unreachable")
	}
}

func (c *Checker) SynthCallExpr(expr *CallExpr) Type {
	var funcTy *FunctionType
	var typeArgs []Type
	if index, ok := expr.Func.(*IndexExpr); ok {
		if gen, ok := c.Synth(index.Expr).(*FunctionType); ok {
			funcTy = gen
			for _, arg := range index.Indices {
				typeArgs = append(typeArgs, c.Synth(arg))
			}
		}
	}
	if funcTy == nil {
		switch ty := c.Synth(expr.Func).(type) {
		case *FunctionType:
			funcTy = ty
		case *BuiltinFunctionType:
			return c.SynthBuiltinFunctionCall(ty, expr)
		case *SliceType:
			if len(expr.Args) != 1 {
				panic("conversion without exactly one argument")
			}
			switch elemType := c.Under(ty.ElemType).(type) {
			case *TypeBuiltin:
				if elemType.Name.Value == "byte" {
					c.CheckAssignableTo(c.Synth(expr.Args[0]), c.BuiltinType("string"))
					return ty
				}
			}
			panic("TODO")
		case *TypeBuiltin:
			if len(expr.Args) != 1 {
				panic("conversion without exactly one argument")
			}
			return c.SynthBuiltinConversion(expr.Args[0], ty)
		case *TypeOfType:
			return c.SynthConversionExpr(&ConversionExpr{Expr: expr.Func, Type: ty.Type})
		default:
			spew.Dump(ty)
			panic("not a function")
		}
	}
	var variadicParam ParameterDecl
	var variadicIndex int = -1
	if len(expr.Args) != len(funcTy.Signature.Params.Params) {
		variadicParam, variadicIndex = funcTy.Signature.GetVariadicParam()
		if variadicIndex != -1 && len(expr.Args) < len(funcTy.Signature.Params.Params) {
			panic("not enough arguments")
		}
	}
	if len(typeArgs) > len(funcTy.Signature.TypeParams.Params) {
		panic("too many type arguments")
	}

	subst := Subst{}
	for _, tyParam := range funcTy.Signature.TypeParams.Params {
		subst[tyParam.Name] = &TypeParam{Name: c.FreshTypeName(), Bound: tyParam.Constraint}
	}
	funcTy = c.ApplySubst(funcTy, subst).(*FunctionType)
	fmt.Printf("subst FunctionType: %v\n", funcTy)

	for i, tyArg := range typeArgs {
		tyParam := funcTy.Signature.TypeParams.Params[i]
		c.TyCtx.AddRelation(RelationSatisfies{Type: tyArg, Constraint: tyParam.Constraint})
		c.TyCtx.AddEq(tyArg, subst[tyParam.Name])
	}
	for _, tyParam := range funcTy.Signature.TypeParams.Params {
		ty := &TypeParam{Name: tyParam.Name, Bound: tyParam.Constraint}
		c.TyCtx.AddRelation(RelationSubtype{Sub: ty, Super: tyParam.Constraint})
	}
	for i, arg := range expr.Args {
		var param ParameterDecl
		if variadicIndex != -1 && i >= variadicIndex {
			param = variadicParam
		} else {
			param = funcTy.Signature.Params.Params[i]
		}

		argTy := c.Synth(arg)

		switch paramTy := param.Type.(type) {
		case *TypeParam:
			c.TyCtx.AddEq(argTy, paramTy)
		default:
			c.CheckAssignableTo(argTy, paramTy)
		}
	}
	returns := []Type{}
	for _, result := range funcTy.Signature.Results.Params {
		returns = append(returns, result.Type)
	}
	switch len(returns) {
	case 0:
		return &TupleType{Elems: []Type{}}
	case 1:
		return returns[0]
	default:
		return &TupleType{Elems: returns}
	}
}

func (c *Checker) SynthConversionExpr(expr *ConversionExpr) Type {
	// TODO check conversion
	return expr.Type
}

func (c *Checker) SynthBuiltinConversion(expr Expr, targetTy *TypeBuiltin) Type {
	exprTy := c.Synth(expr)
	switch exprTy := exprTy.(type) {
	case *UntypedConstantType:
		if exprTy.IsCompatible(targetTy.Name.Value) {
			return targetTy
		}
	case *TypeBuiltin:
		if exprTy.Name.Value == "Pointer" {
			if targetTy.Name.Value == "uintptr" {
				return targetTy
			}
		}
	}
	panic(fmt.Sprintf("cannot convert %v to %v", exprTy, targetTy))
}

func (c *Checker) SynthBuiltinFunctionCall(f *BuiltinFunctionType, expr *CallExpr) Type {
	switch f.Name {
	case "new":
		return c.SynthBuiltinNewCall(expr)
	case "make":
		return c.SynthBuiltinMakeCall(expr)
	case "append":
		return c.SynthBuiltinAppendCall(expr)
	case "len":
		return c.SynthBuiltinLenCall(expr)
	case "panic":
		return c.SynthBuiltinPanicCall(expr)
	default:
		spew.Dump(f)
		panic("unreachable")
	}
}

func (c *Checker) SynthBuiltinNewCall(expr *CallExpr) Type {
	if len(expr.Args) != 1 {
		panic("builtin new() takes exactly one argument")
	}
	argTy, ok := c.Synth(expr.Args[0]).(*TypeOfType)
	if !ok {
		panic("new() with non-type argument")
	}
	return &PointerType{BaseType: argTy.Type}
}

func (c *Checker) SynthBuiltinMakeCall(expr *CallExpr) Type {
	if len(expr.Args) == 0 {
		panic("builtin make() takes at least one argument")
	}
	argTy, ok := c.Synth(expr.Args[0]).(*TypeOfType)
	if !ok {
		panic("make() with non-type argument")
	}
	elemTy := argTy.Type
	switch elemTy.(type) {
	case *SliceType:
	case *MapType:
	case *ChannelType:
	default:
		panic("make() with non-slice, non-map, non-channel type")
	}
	for _, arg := range expr.Args[1:] {
		c.CheckExpr(arg, c.BuiltinType("int"))
	}
	return elemTy
}

func (c *Checker) SynthBuiltinAppendCall(expr *CallExpr) Type {
	if len(expr.Args) < 2 {
		panic("builtin append() takes at least two arguments")
	}
	firstTy := c.Synth(expr.Args[0])
	sliceTy, ok := firstTy.(*SliceType)
	if !ok {
		panic("append() with non-slice type")
	}
	for _, arg := range expr.Args[1:] {
		c.CheckAssignableTo(c.Synth(arg), sliceTy.ElemType)
	}
	return sliceTy
}

func (c *Checker) SynthBuiltinLenCall(expr *CallExpr) Type {
	if len(expr.Args) != 1 {
		panic("builtin len() takes exactly one argument")
	}
	argTy := c.Synth(expr.Args[0])
	switch c.Under(argTy).(type) {
	case *SliceType:
	case *ArrayType:
	case *MapType:
	case *ChannelType:
	default:
		spew.Dump(argTy)
		panic(fmt.Sprintf("len() on incompatible type %v", argTy))
	}
	return c.BuiltinType("int")
}

func (c *Checker) SynthBuiltinPanicCall(expr *CallExpr) Type {
	if len(expr.Args) != 1 {
		panic("builtin panic() takes exactly one argument")
	}
	c.CheckExpr(expr.Args[0], EmptyInterface())
	return &BottomType{}
}

func (c *Checker) SynthNameExpr(expr *NameExpr) Type {
	return c.Lookup(expr.Name)
}

func (c *Checker) SynthLiteralExpr(expr *LiteralExpr) Type {
	// TODO untype literal types
	switch expr.Literal.(type) {
	case *LiteralInt:
		return &UntypedConstantType{Kind: UntypedConstantInt}
	case *LiteralBool:
		return &UntypedConstantType{Kind: UntypedConstantBool}
	case *LiteralString:
		return &UntypedConstantType{Kind: UntypedConstantString}
	case *LiteralFloat:
		return &UntypedConstantType{Kind: UntypedConstantFloat}
	case *LiteralRune:
		return &UntypedConstantType{Kind: UntypedConstantRune}
	default:
		panic("unreachable")
	}
}

func (c *Checker) UntypedDefaultType(ty *UntypedConstantType) Type {
	switch ty.Kind {
	case UntypedConstantInt:
		return c.BuiltinType("int")
	case UntypedConstantBool:
		return c.BuiltinType("bool")
	case UntypedConstantString:
		return c.BuiltinType("string")
	case UntypedConstantFloat:
		return c.BuiltinType("float64")
	case UntypedConstantRune:
		return c.BuiltinType("rune")
	default:
		panic("unreachable")
	}
}

func (c *Checker) SynthCompositeLitExpr(expr *CompositeLitExpr) Type {
	return c.MakeCompositeLit(expr, expr.Type)
}

func (c *Checker) MakeCompositeLit(expr *CompositeLitExpr, targetTy Type) Type {
	targetTy = c.ResolveType(targetTy)

	switch exprTy := c.Under(targetTy).(type) {
	case *StructType:
		return c.MakeCompositeLitStruct(expr, exprTy)
	case *SliceType:
		return c.MakeCompositeLitSlice(expr, exprTy)
	default:
		// TODO MapType, ArrayType
		spew.Dump(expr)
		panic("unreachable")
	}
}

func (c *Checker) MakeCompositeLitStruct(expr *CompositeLitExpr, structTy *StructType) Type {
	if len(expr.Elems) == 0 {
		return expr.Type
	}

	ordered := expr.Elems[0].Key == nil

	if ordered {
		if len(expr.Elems) != len(structTy.Fields) {
			panic("composite literal with wrong number of fields")
		}
		for i, elem := range expr.Elems {
			if elem.Key != nil {
				panic("composite literal with ordered fields")
			}
			field := structTy.Fields[i]
			c.CheckExpr(elem.Value, field.Type)
		}
	} else {
	elems:
		for _, elem := range expr.Elems {
			for _, field := range structTy.Fields {
				if elem.Key == nil {
					panic("composite literal with unordered fields")
				}
				key, ok := elem.Key.(*NameExpr)
				if !ok {
					panic("struct literal must use identifier as key name")
				}
				if field.Name == key.Name {
					c.CheckExpr(elem.Value, field.Type)
					continue elems
				}
			}
			panic(fmt.Sprintf("type %v has no field %v", structTy, elem.Key))
		}
	}

	return expr.Type
}

func (c *Checker) MakeCompositeLitSlice(expr *CompositeLitExpr, sliceTy *SliceType) Type {
	for _, elem := range expr.Elems {
		c.CheckExpr(elem.Value, sliceTy.ElemType)
	}
	return expr.Type
}

func (c *Checker) TypeApplication(app *TypeApplication) Type {
	return c.TypeApplicationFunc(app, func(TypeParamDecl, Type) {})
}

func (c *Checker) TypeApplicationFunc(app *TypeApplication, argF func(tyParam TypeParamDecl, tyArg Type)) Type {
	named, subst := c.InstantiateTypeFunc(app, argF)
	gen := c.ResolveType(named.Type).(*GenericType)
	return c.ApplySubst(gen.Type, subst)
}

func (c *Checker) InstantiateType(app *TypeApplication) (*NamedType, Subst) {
	return c.InstantiateTypeFunc(app, func(TypeParamDecl, Type) {})
}

func (c *Checker) InstantiateTypeFunc(app *TypeApplication, argF func(tyParam TypeParamDecl, tyArg Type)) (*NamedType, Subst) {
	named, ok := c.ResolveType(&app.ID).(*NamedType)
	if !ok {
		panic("can only instantiate named types?")
	}
	gen, ok := c.ResolveType(named.Type).(*GenericType)
	if !ok {
		panic("not a generic type")
	}
	if len(gen.TypeParams.Params) != len(app.Args) {
		panic("wrong number of type arguments")
	}
	subst := Subst{}
	for i, tyArg := range app.Args {
		tyParam := gen.TypeParams.Params[i]
		subst[tyParam.Name] = tyArg
		c.TyCtx.AddRelation(RelationSatisfies{Type: tyArg, Constraint: tyParam.Constraint})
		argF(tyParam, tyArg)
	}
	return named, subst
}

// ========================

type Subst map[Identifier]Type

func (s Subst) String() string {
	parts := []string{}
	for k, v := range s {
		parts = append(parts, fmt.Sprintf("%v -> %v", k, v))
	}
	return fmt.Sprintf("{{ %v }}", strings.Join(parts, " ; "))
}

func (c *Checker) ApplySubst(ty Type, subst Subst) Type {
	switch ty := ty.(type) {
	case *TypeName:
		if substTy, ok := subst[ty.Name]; ok {
			return substTy
		}
		return ty
	case *TypeBuiltin:
		return ty
	case *TypeParam:
		if substTy, ok := subst[ty.Name]; ok {
			return substTy
		}
		return ty
	case *TypeApplication:
		args := make([]Type, len(ty.Args))
		for i, arg := range ty.Args {
			args[i] = c.ApplySubst(arg, subst)
		}
		return &TypeApplication{ID: ty.ID, Args: args}
	case *ArrayType:
		return &ArrayType{
			ElemType: c.ApplySubst(ty.ElemType, subst),
			Len:      ty.Len,
		}
	case *FunctionType:
		return &FunctionType{
			Signature: c.ApplySubstSignature(ty.Signature, subst),
		}
	case *GenericType:
		return &GenericType{
			TypeParams: c.ApplySubstTypeParamList(ty.TypeParams, subst),
			Type:       c.ApplySubst(ty.Type, subst),
		}
	case *StructType:
		fields := make([]FieldDecl, len(ty.Fields))
		for i, field := range ty.Fields {
			fields[i] = FieldDecl{
				Name: field.Name,
				Type: c.ApplySubst(field.Type, subst),
			}
		}
		return &StructType{Fields: fields}
	case *PointerType:
		return &PointerType{BaseType: c.ApplySubst(ty.BaseType, subst)}
	case *InterfaceType:
		constraints := make([]TypeConstraint, len(ty.Constraints))
		for i, constraint := range ty.Constraints {
			constraints[i] = TypeConstraint{TypeElem: c.ApplySubstTypeElem(constraint.TypeElem, subst)}
		}
		return &InterfaceType{
			Methods:     c.ApplySubstMethodList(ty.Methods, subst),
			Constraints: constraints,
		}
	case *SliceType:
		return &SliceType{ElemType: c.ApplySubst(ty.ElemType, subst)}
	case *MapType:
		return &MapType{
			KeyType:  c.ApplySubst(ty.KeyType, subst),
			ElemType: c.ApplySubst(ty.ElemType, subst),
		}
	case *ChannelType:
		return &ChannelType{
			ElemType: c.ApplySubst(ty.ElemType, subst),
			Dir:      ty.Dir,
		}
	case *TupleType:
		elems := make([]Type, len(ty.Elems))
		for i, elem := range ty.Elems {
			elems[i] = c.ApplySubst(elem, subst)
		}
		return &TupleType{Elems: elems}
	case *NamedType:
		return &NamedType{
			Name: ty.Name,
			Type: c.ApplySubst(ty.Type, subst),
			// Methods: c.ApplySubstMethodList(ty.Methods, subst),
		}
	case *NilType:
		return ty
	case *UntypedConstantType:
		return ty
	case *TypeOfType:
		return &TypeOfType{Type: c.ApplySubst(ty.Type, subst)}
	case *QualIdentifier:
		return ty
	default:
		spew.Dump(ty)
		panic("unreachable")
	}
}

func (c *Checker) ApplySubstMethodList(methods []MethodElem, subst Subst) []MethodElem {
	out := make([]MethodElem, len(methods))
	for i, method := range methods {
		out[i] = MethodElem{
			Name: method.Name,
			Type: c.ApplySubst(method.Type, subst).(*FunctionType),
		}
	}
	return out
}

func (c *Checker) ApplySubstSignature(sig Signature, subst Subst) Signature {
	return Signature{
		TypeParams: c.ApplySubstTypeParamList(sig.TypeParams, subst),
		Params:     c.ApplySubstParameterList(sig.Params, subst),
		Results:    c.ApplySubstParameterList(sig.Results, subst),
	}
}

func (c *Checker) ApplySubstTypeParamList(list TypeParamList, subst Subst) TypeParamList {
	params := make([]TypeParamDecl, len(list.Params))
	for i, param := range list.Params {
		var name Identifier
		if substTy, ok := subst[param.Name]; ok {
			name = substTy.(*TypeParam).Name
		} else {
			name = param.Name
		}
		params[i] = TypeParamDecl{
			Name:       name,
			Constraint: c.ApplySubst(param.Constraint, subst).(*InterfaceType),
		}
	}
	return TypeParamList{Params: params}
}

func (c *Checker) ApplySubstTypeElem(elem TypeElem, subst Subst) TypeElem {
	union := make([]TypeTerm, len(elem.Union))
	for i, term := range elem.Union {
		union[i] = TypeTerm{Type: c.ApplySubst(term.Type, subst), Tilde: term.Tilde}
	}
	return TypeElem{Union: union}
}

func (c *Checker) ApplySubstParameterList(list ParameterList, subst Subst) ParameterList {
	params := make([]ParameterDecl, len(list.Params))
	for i, param := range list.Params {
		params[i] = ParameterDecl{
			Name:     param.Name,
			Type:     c.ApplySubst(param.Type, subst),
			Variadic: param.Variadic,
		}
	}
	return ParameterList{Params: params}
}

// ========================

func (c *Checker) Simplify(subst Subst) Subst {
	next := Subst{}
	for k, v := range subst {
		next[k] = c.ApplySubst(v, subst)
	}
	return next
}

func (c *Checker) Merge(lhs, rhs Subst) Subst {
	result := Subst{}
	for k, v := range lhs {
		result[k] = v
	}
	for k, v := range rhs {
		if _, ok := result[k]; ok {
			if !c.Identical(result[k], v) {
				spew.Dump(k, v)
				panic("incompatible substitutions")
			}
		}
		result[k] = v
	}
	return result
}

func (c *Checker) Verify() Subst {
	subst := Subst{}

	fmt.Println("=== Verify ===")

	for i := 0; i < 10; i++ {
		fmt.Printf("=== iteration %d ===\n", i)
		fmt.Println(c.TyCtx)

		learned := Subst{}

		c.Unify(c.TyCtx.Relations, learned)
		learned = c.Simplify(learned)

		fmt.Printf("learned: %v\n", learned)

		next := []Relation{}

		for _, rel := range c.TyCtx.Relations {
			switch rel := rel.(type) {
			case RelationEq:
				next = append(next, RelationEq{
					Left:  c.ApplySubst(rel.Left, learned),
					Right: c.ApplySubst(rel.Right, learned),
				})
			case RelationSubtype:
				next = append(next, RelationSubtype{
					Sub:   c.ApplySubst(rel.Sub, learned),
					Super: c.ApplySubst(rel.Super, learned),
				})
			case RelationSatisfies:
				next = append(next, RelationSatisfies{
					Type:       c.ApplySubst(rel.Type, learned),
					Constraint: c.ApplySubst(rel.Constraint, learned).(*InterfaceType),
				})
			default:
				panic("unreachable")
			}
		}

		c.TyCtx.Relations = next
		subst = c.Merge(subst, learned)

		if len(learned) == 0 {
			break
		}
	}

	subst = c.Simplify(subst)

	fmt.Println("=== subst ===")
	fmt.Println(subst)

	return subst
}

func (c *Checker) Unify(rels []Relation, subst Subst) {
	for _, rel := range rels {
		switch rel := rel.(type) {
		case RelationEq:
			c.UnifyEq(rel.Left, rel.Right, subst)
		case RelationSubtype:
			c.UnifySubtype(rel.Sub, rel.Super, subst)
		case RelationSatisfies:
			c.UnifySatisfies(rel.Type, rel.Constraint, subst)
		default:
			panic("unreachable")
		}
	}
}

func IntersectInterfaces(elems ...*InterfaceType) *InterfaceType {
	inter := &InterfaceType{Methods: nil, Constraints: nil}
	for _, elem := range elems {
		inter.Methods = append(inter.Methods, elem.Methods...)
		inter.Constraints = append(inter.Constraints, elem.Constraints...)
	}
	return inter
}

func (c *Checker) UnifyEq(left, right Type, subst Subst) {
	left = c.ResolveType(left)
	right = c.ResolveType(right)

	fmt.Printf("? %v = %v %v\n", left, right, subst)

	if c.Identical(left, right) {
		return
	}

	if _, ok := right.(*TypeParam); ok {
		left, right = right, left
	}

	if _, ok := left.(*NilType); ok {
		left, right = right, left
	}

	if _, ok := right.(*NilType); ok {
		switch left := left.(type) {
		case *PointerType:
			return
		case *ChannelType:
			return
		case *FunctionType:
			return
		case *InterfaceType:
			return
		case *MapType:
			return
		case *SliceType:
			return
		case *TypeBuiltin:
			if left.Name.Value == "Pointer" {
				return
			}
			panic(fmt.Sprintf("cannot assign nil to type %v", left))
		default:
			panic(fmt.Sprintf("cannot assign nil to type %v", left))
		}
	}

	switch left := left.(type) {
	case *TypeBuiltin:
		if _, ok := right.(*TypeBuiltin); ok {
			panic(fmt.Sprintf("cannot unify: %v = %v", left, right))
		}
		c.UnifyEq(right, left, subst)
	case *TypeParam:
		if right, ok := right.(*UntypedConstantType); ok {
			c.UnifyEq(left, c.UntypedDefaultType(right), subst)
			return
		}
		if s, ok := subst[left.Name]; ok {
			if !c.Identical(s, right) {
				c.UnifyEq(s, right, subst)
			}
		} else {
			subst[left.Name] = right
		}
	case *SliceType:
		if right, ok := right.(*SliceType); ok {
			c.UnifyEq(left.ElemType, right.ElemType, subst)
			return
		}
	case *InterfaceType:
		left = c.SimplifyInterface(left)
		if single, ok := IsSingleTypeUnion(left); ok {
			c.UnifyEq(single, right, subst)
			return
		}
		if _, ok := right.(*NilType); ok {
			return
		}
		spew.Dump(left, right)
		panic("TODO")
	case *TypeApplication:
		if right, ok := right.(*TypeApplication); ok {
			if left.ID != right.ID {
				panic(fmt.Sprintf("cannot unify: %v = %v", left, right))
			}
			if len(left.Args) != len(right.Args) {
				panic(fmt.Sprintf("cannot unify: %v = %v", left, right))
			}
			for i, leftArg := range left.Args {
				c.UnifyEq(leftArg, right.Args[i], subst)
			}
			return
		}
		panic(fmt.Sprintf("cannot unify: %v = %v", left, right))
	case *PointerType:
		if right, ok := right.(*PointerType); ok {
			c.UnifyEq(left.BaseType, right.BaseType, subst)
			return
		}
		c.UnifyEq(right, left, subst) // TODO weird?
	case *NamedType:
		c.UnifyEq(c.Under(left), c.Under(right), subst)
	case *UntypedConstantType:
		if right, ok := right.(*TypeBuiltin); ok {
			if left.IsCompatible(right.Name.Value) {
				return
			}
		}
		c.UnifyEq(c.UntypedDefaultType(left), right, subst)
	default:
		spew.Dump(left, right)
		panic("unreachable")
	}
}

func (c *Checker) UnifySubtype(sub, super Type, subst Subst) {
	sub = c.ResolveType(sub)
	super = c.ResolveType(super)

	fmt.Printf("? %v <: %v %v\n", sub, super, subst)

	if sub, ok := c.ResolveType(sub).(*TypeParam); ok {
		if !c.Identical(sub, super) && c.ContainsTypeParam(super, sub) {
			panic(fmt.Sprintf("circular constraint: %v <: %v", sub, super))
		}
	}
	if super, ok := c.ResolveType(super).(*TypeParam); ok {
		if !c.Identical(sub, super) && c.ContainsTypeParam(sub, super) {
			panic(fmt.Sprintf("circular constraint: %v <: %v", sub, super))
		}
	}

	if _, ok := sub.(*NilType); ok {
		switch super := c.Under(super).(type) {
		case *PointerType:
			return
		case *ChannelType:
			return
		case *FunctionType:
			return
		case *InterfaceType:
			return
		case *MapType:
			return
		case *SliceType:
			return
		case *TypeBuiltin:
			if super.Name.Value == "Pointer" {
				return
			}
			panic(fmt.Sprintf("cannot assign nil to type %v", super))
		default:
			panic(fmt.Sprintf("cannot assign nil to type %v", super))
		}
	}

	if c.IsConcreteType(super) {
		c.UnifyEq(sub, super, subst)
		return
	}

	switch super := super.(type) {
	case *InterfaceType:
		var typeset *TypeSet
		c.BasicSatisfy(sub, super, subst, &typeset)
		if typeset != nil && !typeset.Universe {
			// TODO hacky???
			panic(fmt.Sprintf("cannot assign %v to %v", sub, super))
		}
	case *TypeApplication:
		c.UnifySubtype(sub, c.Under(super), subst) // TODO: adding more constraints?
	case *NamedType:
		if subTy, ok := sub.(*NamedType); ok {
			if subTy.Name == super.Name {
				return
			}
		}
		c.UnifySubtype(sub, c.Under(super), subst)
	case *TypeParam:
		if subTy, ok := c.ResolveType(sub).(*TypeParam); ok {
			if subTy.Name == super.Name {
				return
			}
		}
		if super.Bound != nil {
			c.UnifySubtype(sub, super.Bound, subst)
		}
	default:
		spew.Dump(sub, super)
		panic("unreachable")
	}
}

func (c *Checker) UnifySatisfies(sub Type, inter *InterfaceType, subst Subst) {
	sub = c.ResolveType(sub)

	var typeset *TypeSet
	c.BasicSatisfy(sub, inter, subst, &typeset)

	if typeset != nil && !typeset.Universe {
		for _, term := range typeset.Types {
			termTy := c.ResolveType(term)
			if !c.IsConcreteType(termTy) {
				panic("cannot make union of non-concrete types")
			}
			if c.Identical(sub, termTy) {
				c.UnifyEq(sub, termTy, subst) // necessary?
				return
			}
		}
		panic(fmt.Sprintf("type %v does not satisfy %v", sub, inter))
	}
}

// TODO this seems unprincipled
func (c *Checker) BasicSatisfy(sub Type, inter *InterfaceType, subst Subst, out **TypeSet) {
	inter = c.SimplifyInterface(inter)
	supertypeset := c.InterfaceTypeSet(inter)
	if !supertypeset.Universe && len(supertypeset.Types) == 0 {
		panic("cannot satisfy empty set")
	}
	if len(supertypeset.Types) == 1 {
		single := c.ResolveType(supertypeset.Types[0])
		// TODO hacky
		if c.Identical(sub, single) {
			return
		}
		if c.IsConcreteType(single) {
			c.UnifyEq(sub, single, subst)
		}
		// c.UnifySatisfies(sub, &InterfaceType{Methods: supertypeset.Methods}, subst)
		c.TyCtx.AddRelation(RelationSubtype{Sub: sub, Super: &InterfaceType{Methods: supertypeset.Methods}})
		return // leave for next iteration?
	}
	if len(supertypeset.Methods) > 0 {
		subMethods, pointerReceiver := c.MethodSet(sub)
	super:
		for _, superMethod := range supertypeset.Methods {
			for _, subMethod := range subMethods {
				if subMethod.Name == superMethod.Name {
					if subMethod.PointerReceiver && !pointerReceiver {
						panic(fmt.Sprintf("cannot use pointer-receiver method %v with non pointer", subMethod))
					}
					if c.Identical(subMethod.Type, superMethod.Type) {
						continue super
					}
					panic("incompatible method signature")
				}
			}
			panic(fmt.Sprintf("type %v doesn't have method %v", sub, superMethod))
		}
	}
	if tyPar, ok := c.ResolveType(sub).(*TypeParam); ok {
		var bound Type = tyPar.Bound
		if tyPar.Bound == nil {
			bound = EmptyInterface()
		}
		c.UnifySubtype(bound, inter, subst)
		return
	}
	if sub, ok := c.ResolveType(sub).(*InterfaceType); ok {
		subtypeset := c.InterfaceTypeSet(sub)
		if len(subtypeset.Methods) != 0 {
			panic("TODO")
		}
		for _, term := range subtypeset.Types {
			termTy := c.ResolveType(term)
			found := false
			for _, superTerm := range supertypeset.Types {
				superTermTy := c.ResolveType(superTerm)
				if c.Identical(termTy, superTermTy) {
					found = true
				}
			}
			if !found {
				panic(fmt.Sprintf("interface %v does not satisfy %v", sub, inter))
			}
		}
		return
	}
	if len(supertypeset.Types) == 1 {
		c.UnifySubtype(sub, supertypeset.Types[0], subst)
		return
	}
	*out = &supertypeset
}

func (c *Checker) Under(ty Type) Type {
	switch ty := ty.(type) {
	case *NamedType:
		return c.Under(ty.Type)
	case *TypeApplication:
		// TODO: pre apply?
		return c.TypeApplication(ty) // TODO crazy?
	default:
		return ty
	}
}

func (c *Checker) ContainsTypeParam(ty Type, tyParam *TypeParam) bool {
	switch ty := ty.(type) {
	case *TypeParam:
		return ty.Name == tyParam.Name
	case *TypeApplication:
		for _, arg := range ty.Args {
			if c.ContainsTypeParam(arg, tyParam) {
				return true
			}
		}
		return false
	case *PointerType:
		return c.ContainsTypeParam(ty.BaseType, tyParam)
	case *StructType:
		for _, field := range ty.Fields {
			if c.ContainsTypeParam(field.Type, tyParam) {
				return true
			}
		}
		return false
	case *InterfaceType:
		for _, constraint := range ty.Constraints {
			for _, term := range constraint.TypeElem.Union {
				if c.ContainsTypeParam(term.Type, tyParam) {
					return true
				}
			}
		}
		for _, method := range ty.Methods {
			if c.ContainsTypeParam(method.Type, tyParam) {
				return true
			}
		}
		return false
	case *SliceType:
		return c.ContainsTypeParam(ty.ElemType, tyParam)
	case *TypeBuiltin:
		return false
	case *ArrayType:
		return c.ContainsTypeParam(ty.ElemType, tyParam)
	case *FunctionType:
		// could check signature type params, but no nested type params?
		for _, param := range ty.Signature.Params.Params {
			if c.ContainsTypeParam(param.Type, tyParam) {
				return true
			}
		}
		for _, result := range ty.Signature.Results.Params {
			if c.ContainsTypeParam(result.Type, tyParam) {
				return true
			}
		}
		return false
	case *GenericType:
		// cannot nest generic types?
		return false
	case *TupleType:
		for _, elem := range ty.Elems {
			if c.ContainsTypeParam(elem, tyParam) {
				return true
			}
		}
		return false
	default:
		return false
	}
}

func (c *Checker) MethodSet(ty Type) ([]MethodElem, bool) {
	ty = c.ResolveType(ty)

	var pointerReceiver bool
	if pointerTy, ok := ty.(*PointerType); ok {
		ty = pointerTy.BaseType
		pointerReceiver = true
	}

	switch ty := c.ResolveType(ty).(type) {
	case *InterfaceType:
		return ty.Methods, false
	case *NamedType:

		return c.NamedTypeMethods(ty), pointerReceiver
	case *TypeApplication:
		named, subst := c.InstantiateType(ty)
		methods := []MethodElem{}
		for _, m := range c.NamedTypeMethods(named) {
			methods = append(methods, MethodElem{
				Name: m.Name,
				Type: c.ApplySubst(m.Type, subst).(*FunctionType),
			})
		}
		return methods, pointerReceiver
	default:
		spew.Dump(ty)
		panic(fmt.Sprintf("type %v cannot have methods", ty))
	}
}

func (c *Checker) NamedTypeMethods(namedTy *NamedType) []MethodElem {
	if interTy, ok := c.Under(namedTy).(*InterfaceType); ok {
		return interTy.Methods
	}

	methods := []MethodElem{}

	c.VarCtx.Iter(func(name Identifier, ty Type) {
		if strings.HasPrefix(name.Value, namedTy.Name.Value+".") {
			methodTy := ty.(*MethodType)
			methods = append(methods, MethodElem{
				Name:            Identifier{Value: name.Value[len(namedTy.Name.Value)+1:]},
				Type:            methodTy.Type,
				PointerReceiver: methodTy.PointerReceiver,
			})
		}
	})

	return methods
}

func (c *Checker) IsTypeParam(ty Type) bool {
	switch ty.(type) {
	case *TypeParam:
		return true
	default:
		return false
	}
}

func (c *Checker) IsConcreteType(ty Type) bool {
	switch ty := ty.(type) {
	case *TypeOfType:
		return c.IsConcreteType(ty.Type)
	case *TypeBuiltin:
		return true
	case *InterfaceType:
		return false
	case *TypeParam:
		return false
	case *PointerType:
		return true
	case *SliceType:
		return true
	case *ArrayType:
		return true
	case *StructType:
		return true
	case *TypeApplication:
		return c.IsConcreteType(c.TypeApplication(ty))
	case *NamedType:
		return c.IsConcreteType(ty.Type)
	case *UntypedConstantType:
		return true
	default:
		spew.Dump(ty)
		panic("unreachable")
	}
}

func (c *Checker) Identical(ty1, ty2 Type) bool {
	// fmt.Printf("== Identical(%v, %v) ==\n", ty1, ty2)
	// TODO recursive types?
	switch ty1 := ty1.(type) {
	case *NamedType:
		if ty2, ok := ty2.(*NamedType); ok {
			return ty1.Name == ty2.Name
		}
		return false
	case *TypeBuiltin:
		ty, ok := ty2.(*TypeBuiltin)
		if !ok {
			return false
		}
		return ty1.Name == ty.Name
	case *TypeParam:
		if ty2, ok := ty2.(*TypeParam); ok {
			return ty1.Name == ty2.Name
		}
		return false
	case *InterfaceType:
		if ty2, ok := ty2.(*InterfaceType); ok {
			if len(ty1.Methods) != len(ty2.Methods) {
				return false
			}
			if len(ty1.Constraints) != len(ty2.Constraints) {
				return false
			}
			panic("TODO")
		}
		return false
	case *PointerType:
		if ty2, ok := ty2.(*PointerType); ok {
			return c.Identical(c.ResolveType(ty1.BaseType), c.ResolveType(ty2.BaseType))
		}
		return false
	case *TypeApplication:
		if ty2, ok := ty2.(*TypeApplication); ok {
			if ty1.ID != ty2.ID {
				return false
			}
			if len(ty1.Args) != len(ty2.Args) {
				return false
			}
			for i, arg := range ty1.Args {
				if !c.Identical(c.ResolveType(arg), c.ResolveType(ty2.Args[i])) {
					return false
				}
			}
			return true
		}
		return false
	case *ArrayType:
		if ty2, ok := ty2.(*ArrayType); ok {
			if ty1.Len != ty2.Len {
				return false
			}
			return c.Identical(ty1.ElemType, ty2.ElemType)
		}
		return false
	case *StructType:
		if ty2, ok := ty2.(*StructType); ok {
			if len(ty1.Fields) != len(ty2.Fields) {
				return false
			}
			for i, field := range ty1.Fields {
				if field.Name != ty2.Fields[i].Name {
					return false
				}
				if !c.Identical(field.Type, ty2.Fields[i].Type) {
					return false
				}
			}
			return true
		}
		return false
	case *SliceType:
		if ty2, ok := ty2.(*SliceType); ok {
			return c.Identical(c.ResolveType(ty1.ElemType), c.ResolveType(ty2.ElemType))
		}
		return false
	case *FunctionType:
		if ty2, ok := ty2.(*FunctionType); ok {
			return c.IdenticalFunctionTypes(ty1, ty2)
		}
		return false
	case *UntypedConstantType:
		return false // TODO ???
	case *NilType:
		return false // TODO ???
	default:
		spew.Dump(ty1, ty2)
		panic("unreachable")
	}
}

func (c *Checker) IdenticalFunctionTypes(ty1, ty2 *FunctionType) bool {
	if len(ty1.Signature.TypeParams.Params) != 0 {
		panic("cannot compare type with type parameters")
	}
	if len(ty2.Signature.TypeParams.Params) != 0 {
		panic("cannot compare type with type parameters")
	}
	if len(ty1.Signature.Params.Params) != len(ty2.Signature.Params.Params) {
		return false
	}
	if len(ty1.Signature.Results.Params) != len(ty2.Signature.Results.Params) {
		return false
	}
	for i := range ty1.Signature.Params.Params {
		// TODO variadic
		par1 := ty1.Signature.Params.Params[i]
		par2 := ty2.Signature.Params.Params[i]
		if !c.Identical(c.ResolveType(par1.Type), c.ResolveType(par2.Type)) {
			return false
		}
	}
	for i := range ty1.Signature.Results.Params {
		par1 := ty1.Signature.Results.Params[i]
		par2 := ty2.Signature.Results.Params[i]
		if !c.Identical(c.ResolveType(par1.Type), c.ResolveType(par2.Type)) {
			return false
		}
	}
	return true
}

func (c *Checker) InterfaceTypeSet(ty *InterfaceType) TypeSet {
	typeset := TypeSet{
		Methods:  ty.Methods,
		Universe: true,
	}

	if len(ty.Constraints) == 0 {
		return typeset
	}

	for _, constraint := range ty.Constraints {
		var next TypeSet
		if len(constraint.TypeElem.Union) == 1 {
			term := constraint.TypeElem.Union[0]
			if term.Tilde {
				panic("TODO")
			}
			termTy := c.ResolveType(term.Type)
			switch underTy := c.Under(termTy).(type) {
			case *InterfaceType:
				next = c.InterfaceTypeSet(underTy)
			case *TypeParam:
				if underTy.Bound != nil {
					next = c.InterfaceTypeSet(underTy.Bound)
				} else {
					next = TypeSet{Types: []Type{underTy}, Universe: false}
				}
			default:
				next = TypeSet{Types: []Type{termTy}, Universe: false}
			}
		} else {
			var types []Type
			for _, term := range constraint.TypeElem.Union {
				if term.Tilde {
					panic("TODO")
				}
				termTy := c.ResolveType(term.Type)
				switch ty := c.Under(termTy).(type) {
				case *InterfaceType:
					if len(ty.Methods) == 0 {
						spew.Dump(ty)
						panic("cannot make union of interface with methods")
					}
					spew.Dump(ty)
					panic("what")
				default:
					types = append(types, ty)
				}
			}
			next = TypeSet{Types: types, Universe: false}
		}
		typeset = c.Combine(typeset, next)
	}

	return typeset
}

func (c *Checker) TypeSet(con TypeConstraint) TypeSet {
	if len(con.TypeElem.Union) == 1 {
		term := con.TypeElem.Union[0]
		if term.Tilde {
			panic("TODO")
		}
		termTy := c.ResolveType(term.Type)
		switch ty := c.Under(termTy).(type) {
		case *InterfaceType:
			return c.InterfaceTypeSet(ty)
		case *TypeParam:
			if ty.Bound != nil {
				return c.InterfaceTypeSet(ty.Bound)
			}
			return TypeSet{Types: []Type{ty}, Universe: false}
		default:
			return TypeSet{Types: []Type{ty}, Universe: false}
		}
	}
	var types []Type
	for _, term := range con.TypeElem.Union {
		if term.Tilde {
			panic("TODO")
		}
		termTy := c.ResolveType(term.Type)
		switch ty := c.Under(termTy).(type) {
		case *InterfaceType:
			if len(ty.Methods) == 0 {
				spew.Dump(ty)
				panic("cannot make union of interface with methods")
			}
		default:
			types = append(types, ty)
		}
	}
	return TypeSet{Types: types, Universe: false}
}

func (c *Checker) SimplifyInterface(ty *InterfaceType) *InterfaceType {
	if single, ok := IsSingleTypeUnion(ty); ok {
		switch single := single.(type) {
		case *InterfaceType:
			return c.SimplifyInterface(single)
		case *TypeName:
			singleRef := c.Lookup(single.Name)
			if singleRef, ok := singleRef.(*InterfaceType); ok {
				return c.SimplifyInterface(singleRef)
			}
		}
	}
	return ty
}

func IsSingleTypeUnion(ty *InterfaceType) (Type, bool) {
	if len(ty.Methods) == 0 && len(ty.Constraints) == 1 && len(ty.Constraints[0].TypeElem.Union) == 1 {
		return ty.Constraints[0].TypeElem.Union[0].Type, true
	}
	return nil, false
}

// ========================

func ReadAST(path string, file *ast.File) File {
	var decls []Decl
	for _, decl := range file.Decls {
		decls = append(decls, ReadDecl(decl)...)
	}
	return File{
		Path:  path,
		Decls: decls,
	}
}

func ReadDecl(decl ast.Decl) []Decl {
	switch decl := decl.(type) {
	case *ast.GenDecl:
		return ReadGenDecl(decl)
	case *ast.FuncDecl:
		return ReadFuncDecl(decl)
	default:
		panic("unreachable")
	}
}

func ReadGenDecl(decl *ast.GenDecl) []Decl {
	switch decl.Tok {
	case token.CONST:
		return ReadConstDecl(decl)
	case token.TYPE:
		return ReadTypeDecl(decl)
	case token.VAR:
		return ReadVarDecl(decl)
	case token.IMPORT:
		return ReadImport(decl)
	default:
		spew.Dump(decl)
		panic("unreachable")
	}
}

func ReadImport(decl *ast.GenDecl) []Decl {
	var decls []Decl
	for _, spec := range decl.Specs {
		spec := spec.(*ast.ImportSpec)
		var name *Identifier
		if spec.Name != nil {
			name = Ptr(NewIdentifier(spec.Name.Name))
		}
		path := spec.Path.Value
		path = path[1 : len(path)-1] // remove quotes
		decls = append(decls, &ImportDecl{
			Path:  path,
			Alias: name,
		})
	}
	return decls
}

func ReadConstDecl(decl *ast.GenDecl) []Decl {
	var elems []ConstDeclElem
	for _, spec := range decl.Specs {
		spec := spec.(*ast.ValueSpec)
		for i, name := range spec.Names {
			var value Expr
			if spec.Values != nil {
				value = ReadExpr(spec.Values[i])
			}
			elems = append(elems, ConstDeclElem{
				Name:  NewIdentifier(name.Name),
				Type:  ReadType(spec.Type),
				Value: value,
			})
		}
	}
	return []Decl{&ConstDecl{Elems: elems}}
}

func ReadTypeDecl(decl *ast.GenDecl) []Decl {
	var decls []Decl
	for _, spec := range decl.Specs {
		spec := spec.(*ast.TypeSpec)
		decls = append(decls, &TypeDecl{
			Name:       NewIdentifier(spec.Name.Name),
			TypeParams: ReadTypeParamList(spec.TypeParams),
			Type:       ReadType(spec.Type),
		})
	}
	return decls
}

func ReadVarDecl(decl *ast.GenDecl) []Decl {
	var decls []Decl
	for _, spec := range decl.Specs {
		spec := spec.(*ast.ValueSpec)
		for i, name := range spec.Names {
			var expr Expr
			if spec.Values != nil {
				expr = ReadExpr(spec.Values[i])
			}
			decls = append(decls, &VarDecl{
				Name: NewIdentifier(name.Name),
				Type: ReadType(spec.Type),
				Expr: expr,
			})
		}
	}
	return decls
}

func ReadFuncDecl(decl *ast.FuncDecl) []Decl {
	if decl.Recv == nil {
		return []Decl{&FunctionDecl{
			Name:      NewIdentifier(decl.Name.Name),
			Signature: ReadSignature(decl.Type),
			Body:      ReadStatementList(decl.Body),
		}}
	} else {
		return []Decl{&MethodDecl{
			Receiver:  ReadReceiver(decl.Recv.List[0]),
			Name:      NewIdentifier(decl.Name.Name),
			Signature: ReadSignature(decl.Type),
			Body:      ReadStatementList(decl.Body),
		}}

	}
}

func ReadReceiver(field *ast.Field) FieldDecl {
	switch len(field.Names) {
	case 0:
		return FieldDecl{
			Name: IgnoreIdent,
			Type: ReadType(field.Type),
		}
	case 1:
		return FieldDecl{
			Name: NewIdentifier(field.Names[0].Name),
			Type: ReadType(field.Type),
		}
	default:
		panic("too many names for receiver declaration")
	}
}

func ReadTypeParamList(list *ast.FieldList) TypeParamList {
	if list == nil {
		return TypeParamList{}
	}
	var params []TypeParamDecl
	for _, field := range list.List {
		for _, name := range field.Names {
			params = append(params, TypeParamDecl{
				Name:       NewIdentifier(name.Name),
				Constraint: ReadTypeConstraint(field.Type),
			})
		}
	}
	return TypeParamList{Params: params}
}

func ReadTypeConstraint(expr ast.Expr) *InterfaceType {
	switch expr := expr.(type) {
	case *ast.Ident:
		return &InterfaceType{
			Methods: nil,
			Constraints: []TypeConstraint{
				{
					TypeElem{
						Union: []TypeTerm{
							{Type: &TypeName{Name: NewIdentifier(expr.Name)}},
						},
					},
				},
			},
		}
	case *ast.BinaryExpr:
		if expr.Op != token.OR {
			panic("Expected OR")
		}
		terms := []TypeTerm{}
		cur := expr
		for {
			terms = append(terms, ReadUnionTerm(cur.Y))
			if next, ok := cur.X.(*ast.BinaryExpr); ok {
				cur = next
			} else {
				terms = append(terms, ReadUnionTerm(cur.X))
				break
			}
		}
		slices.Reverse(terms)
		return &InterfaceType{
			Methods: nil,
			Constraints: []TypeConstraint{
				{
					TypeElem{
						Union: terms,
					},
				},
			},
		}
	case *ast.InterfaceType:
		return ReadInterfaceType(expr)
	default:
		return &InterfaceType{
			Methods: nil,
			Constraints: []TypeConstraint{
				{
					TypeElem{
						Union: []TypeTerm{
							ReadUnionTerm(expr),
						},
					},
				},
			},
		}
	}
}

func ReadUnionTerm(expr ast.Expr) TypeTerm {
	switch expr := expr.(type) {
	case *ast.UnaryExpr:
		if expr.Op != token.TILDE {
			panic("Expected TILDE")
		}
		return TypeTerm{Type: ReadType(expr.X), Tilde: true}
	default:
		return TypeTerm{Type: ReadType(expr)}
	}
}

func ReadSignature(sig *ast.FuncType) Signature {
	return Signature{
		TypeParams: ReadTypeParamList(sig.TypeParams),
		Params:     ReadParameterList(sig.Params),
		Results:    ReadResultsList(sig.Results),
	}
}

func ReadParameterList(list *ast.FieldList) ParameterList {
	if list == nil {
		return ParameterList{}
	}

	var params []ParameterDecl
	var foundVariadic bool = false

	addParam := func(name Identifier, fieldType ast.Expr) {
		if foundVariadic {
			panic("variadic parameter must be last")
		}
		var ty Type
		var variadic bool
		if ellipsis, ok := fieldType.(*ast.Ellipsis); ok {
			ty = ReadType(ellipsis.Elt)
			variadic = true
			foundVariadic = true
		} else {
			ty = ReadType(fieldType)
			variadic = false
		}
		params = append(params, ParameterDecl{
			Name:     name,
			Type:     ty,
			Variadic: variadic,
		})
	}

	for _, field := range list.List {
		if len(field.Names) == 0 {
			addParam(IgnoreIdent, field.Type)
		}
		for _, name := range field.Names {
			addParam(NewIdentifier(name.Name), field.Type)
		}
	}
	return ParameterList{Params: params}
}

func ReadResultsList(list *ast.FieldList) ParameterList {
	if list == nil {
		return ParameterList{}
	}
	var params []ParameterDecl
	for _, field := range list.List {
		if len(field.Names) == 0 {
			params = append(params, ParameterDecl{
				Name: IgnoreIdent,
				Type: ReadType(field.Type),
			})
		}
		for _, name := range field.Names {
			params = append(params, ParameterDecl{
				Name: NewIdentifier(name.Name),
				Type: ReadType(field.Type),
			})
		}
	}
	return ParameterList{Params: params}
}

func ReadBlockStmt(block *ast.BlockStmt) Statement {
	return &BlockStmt{Body: ReadStatementList(block)}
}

func ReadStatementList(block *ast.BlockStmt) StatementList {
	if block == nil {
		return StatementList{}
	}
	var stmts []Statement
	for _, stmt := range block.List {
		stmts = append(stmts, ReadStmt(stmt))
	}
	return StatementList{Stmts: stmts}
}

func ReadStmt(stmt ast.Stmt) Statement {
	switch stmt := stmt.(type) {
	case *ast.DeclStmt:
		return ReadDeclStmt(stmt)
	case *ast.ExprStmt:
		return ReadExprStmt(stmt)
	case *ast.ReturnStmt:
		return ReadReturnStmt(stmt)
	case *ast.IfStmt:
		return ReadIfStmt(stmt)
	case *ast.AssignStmt:
		return ReadAssignStmt(stmt)
	case *ast.EmptyStmt:
		return &EmptyStmt{}
	case *ast.RangeStmt:
		return ReadRangeStmt(stmt)
	case *ast.IncDecStmt:
		return ReadIncDecStmt(stmt)
	case *ast.TypeSwitchStmt:
		return ReadTypeSwitchStmt(stmt)
	case *ast.SwitchStmt:
		return ReadSwitchStmt(stmt)
	case *ast.BranchStmt:
		return ReadBranchStmt(stmt)
	case *ast.ForStmt:
		return ReadForStmt(stmt)
	case *ast.GoStmt:
		return &GoStmt{Call: ReadCallExpr(stmt.Call).(*CallExpr)}
	case *ast.DeferStmt:
		return &DeferStmt{Call: ReadCallExpr(stmt.Call).(*CallExpr)}
	case *ast.LabeledStmt:
		// TODO labeled satement semantics?
		return ReadStmt(stmt.Stmt)
	case *ast.SendStmt:
		if stmt.Value == nil {
			return &ReceiveStmt{
				Chan: ReadExpr(stmt.Chan),
			}
		} else {
			return &SendStmt{
				Chan:  ReadExpr(stmt.Chan),
				Value: ReadExpr(stmt.Value),
			}
		}
	case *ast.SelectStmt:
		return &SelectStmt{Cases: ReadCommCases(stmt.Body.List)}
	case *ast.BlockStmt:
		return ReadBlockStmt(stmt)
	default:
		spew.Dump(stmt)
		panic("unreachable")
	}
}

func ReadStmtList(stmts []ast.Stmt) StatementList {
	var result []Statement
	for _, stmt := range stmts {
		result = append(result, ReadStmt(stmt))
	}
	return StatementList{Stmts: result}
}

func ReadDeclStmt(stmt *ast.DeclStmt) Statement {
	return &DeclStmt{Decl: ReadDecl(stmt.Decl)[0]}
}

func ReadExprStmt(stmt *ast.ExprStmt) Statement {
	return &ExpressionStmt{Expr: ReadExpr(stmt.X)}
}

func ReadReturnStmt(stmt *ast.ReturnStmt) Statement {
	var results []Expr
	for _, result := range stmt.Results {
		results = append(results, ReadExpr(result))
	}
	return &ReturnStmt{Results: results}
}

func ReadIfStmt(stmt *ast.IfStmt) Statement {
	var init Statement
	if stmt.Init != nil {
		init = ReadStmt(stmt.Init)
	}
	var elseStmt *IfStmt
	if stmt.Else != nil {
		elseStmt = ReadElseStmt(stmt.Else).(*IfStmt)
	}
	return &IfStmt{
		Init: init,
		Cond: ReadExpr(stmt.Cond),
		Body: ReadStatementList(stmt.Body),
		Else: elseStmt,
	}
}

func ReadElseStmt(stmt ast.Stmt) Statement {
	switch stmt := stmt.(type) {
	case *ast.BlockStmt:
		return &IfStmt{
			Cond: nil,
			Body: ReadStatementList(stmt),
		}
	case *ast.IfStmt:
		return ReadIfStmt(stmt)
	default:
		spew.Dump(stmt)
		panic("unreachable")
	}
}

func ReadAssignStmt(stmt *ast.AssignStmt) Statement {
	if stmt.Tok == token.DEFINE {
		names := []Identifier{}
		exprs := []Expr{}
		for _, left := range stmt.Lhs {
			switch left := left.(type) {
			case *ast.Ident:
				names = append(names, NewIdentifier(left.Name))
			default:
				panic("TODO")
			}
		}
		for _, right := range stmt.Rhs {
			exprs = append(exprs, ReadExpr(right))
		}
		return &ShortVarDecl{Names: names, Exprs: exprs}
	} else {
		var lhs []Expr
		var rhs []Expr
		for _, left := range stmt.Lhs {
			lhs = append(lhs, ReadExpr(left))
		}
		for _, right := range stmt.Rhs {
			rhs = append(rhs, ReadExpr(right))
		}
		return &AssignmentStmt{LHS: lhs, RHS: rhs}
	}
}

func ReadRangeStmt(stmt *ast.RangeStmt) Statement {
	return &RangeStmt{
		Assign: stmt.Tok == token.DEFINE,
		Key:    ReadExpr(stmt.Key),
		Value:  ReadExpr(stmt.Value),
		X:      ReadExpr(stmt.X),
		Body:   ReadStatementList(stmt.Body),
	}
}

func ReadIncDecStmt(stmt *ast.IncDecStmt) Statement {
	return &IncDecStmt{
		Expr: ReadExpr(stmt.X),
		Inc:  stmt.Tok == token.INC,
	}
}

func ReadCommCases(clauses []ast.Stmt) []SelectCase {
	var cases []SelectCase
	for _, clause := range clauses {
		clause := clause.(*ast.CommClause)
		var comm Statement
		if clause.Comm != nil {
			comm = ReadStmt(clause.Comm)
		}
		cases = append(cases, SelectCase{
			Comm: comm,
			Body: ReadStmtList(clause.Body),
		})
	}
	return cases
}

func ReadTypeSwitchStmt(stmt *ast.TypeSwitchStmt) Statement {
	var init Statement
	if stmt.Init != nil {
		init = ReadStmt(stmt.Init)
	}
	return &TypeSwitchStmt{
		Init:   init,
		Assign: ReadStmt(stmt.Assign),
		Body:   ReadTypeSwitchCases(stmt.Body.List),
	}
}

func ReadTypeSwitchCases(clauses []ast.Stmt) []TypeSwitchCase {
	var cases []TypeSwitchCase
	for _, clause := range clauses {
		clause := clause.(*ast.CaseClause)
		var types []Type
		for _, expr := range clause.List {
			types = append(types, ReadType(expr))
		}
		cases = append(cases, TypeSwitchCase{
			Types: types,
			Body:  ReadStmtList(clause.Body),
		})
	}
	return cases
}

func ReadSwitchStmt(stmt *ast.SwitchStmt) Statement {
	var init Statement
	var tag Expr
	if stmt.Init != nil {
		init = ReadStmt(stmt.Init)
	}
	if stmt.Tag != nil {
		tag = ReadExpr(stmt.Tag)
	}
	return &SwitchStmt{
		Init:  init,
		Tag:   tag,
		Cases: ReadSwitchCases(stmt.Body.List),
	}
}

func ReadSwitchCases(clauses []ast.Stmt) []SwitchCase {
	var cases []SwitchCase
	for _, clause := range clauses {
		clause := clause.(*ast.CaseClause)
		var exprs []Expr
		for _, expr := range clause.List {
			exprs = append(exprs, ReadExpr(expr))
		}
		cases = append(cases, SwitchCase{
			Exprs: exprs,
			Body:  ReadStmtList(clause.Body),
		})
	}
	return cases
}

func ReadBranchStmt(stmt *ast.BranchStmt) Statement {
	// TOOD matters?
	switch stmt.Tok {
	case token.BREAK:
		return &BranchStmt{}
	case token.CONTINUE:
		return &BranchStmt{}
	case token.FALLTHROUGH:
		return &BranchStmt{}
	case token.GOTO:
		return &BranchStmt{}
	default:
		panic("unreachable")
	}
}

func ReadForStmt(stmt *ast.ForStmt) Statement {
	var init, post Statement
	var cond Expr
	if stmt.Init != nil {
		init = ReadStmt(stmt.Init)
	}
	if stmt.Post != nil {
		post = ReadStmt(stmt.Post)
	}
	if stmt.Cond != nil {
		cond = ReadExpr(stmt.Cond)
	}
	return &ForStmt{
		Init: init,
		Cond: cond,
		Post: post,
		Body: ReadStatementList(stmt.Body),
	}
}

func ReadExpr(expr ast.Expr) Expr {
	switch expr := expr.(type) {
	case *ast.BinaryExpr:
		return ReadBinaryExpr(expr)
	case *ast.UnaryExpr:
		return ReadUnaryExpr(expr)
	case *ast.CallExpr:
		return ReadCallExpr(expr)
	case *ast.Ident:
		return ReadNameExpr(expr)
	case *ast.BasicLit:
		return &LiteralExpr{Literal: ReadLiteral(expr)}
	case *ast.IndexExpr:
		return &IndexExpr{
			Expr:    ReadExpr(expr.X),
			Indices: []Expr{ReadExpr(expr.Index)},
		}
	case *ast.IndexListExpr:
		return &IndexExpr{
			Expr:    ReadExpr(expr.X),
			Indices: ReadExprList(expr.Indices),
		}
	case *ast.StarExpr:
		return &UnaryExpr{
			Op:   UnaryOpDeref,
			Expr: ReadExpr(expr.X),
		}
	case *ast.ParenExpr:
		return ReadExpr(expr.X)
	case *ast.CompositeLit:
		return ReadCompositeLit(expr)
	case *ast.SelectorExpr:
		return &SelectorExpr{
			Expr: ReadExpr(expr.X),
			Sel:  NewIdentifier(expr.Sel.Name),
		}
	case *ast.TypeAssertExpr:
		return &TypeAssertionExpr{
			Expr: ReadExpr(expr.X),
			Type: ReadType(expr.Type),
		}
	case *ast.SliceExpr:
		var low, high, max Expr
		if expr.Low != nil {
			low = ReadExpr(expr.Low)
		}
		if expr.High != nil {
			high = ReadExpr(expr.High)
		}
		if expr.Max != nil {
			max = ReadExpr(expr.Max)
		}
		return &SliceExpr{
			Expr: ReadExpr(expr.X),
			Low:  low,
			High: high,
			Max:  max,
		}
	case *ast.FuncLit:
		return ReadFuncLit(expr)
	case *ast.Ellipsis:
		return &EllipsisExpr{}
	default:
		ty, err := Try(func() Expr {
			return &TypeExpr{Type: ReadType(expr)}
		})
		if err == nil {
			return ty
		}
		spew.Dump(err)
		spew.Dump(expr)
		panic("unreachable")
	}
}

func Try[T any](f func() T) (out T, err error) {
	done := make(chan struct{})
	go func() {
		defer func() {
			if r := recover(); r != nil {
				err = fmt.Errorf("%v", r)
				close(done)
			}
		}()
		out = f()
		close(done)
	}()
	<-done
	return
}

func ReadExprList(exprs []ast.Expr) []Expr {
	var result []Expr
	for _, expr := range exprs {
		result = append(result, ReadExpr(expr))
	}
	return result
}

func ReadBinaryExpr(expr *ast.BinaryExpr) Expr {
	return &BinaryExpr{
		Op:    ReadBinaryOp(expr.Op),
		Left:  ReadExpr(expr.X),
		Right: ReadExpr(expr.Y),
	}
}

func ReadUnaryExpr(expr *ast.UnaryExpr) Expr {
	return &UnaryExpr{
		Op:   ReadUnaryOp(expr.Op),
		Expr: ReadExpr(expr.X),
	}
}

func ReadFuncLit(expr *ast.FuncLit) Expr {
	return &FuncLitExpr{
		Signature: ReadSignature(expr.Type),
		Body:      ReadStatementList(expr.Body),
	}
}

func ReadBinaryOp(op token.Token) BinaryOp {
	switch op {
	case token.ADD:
		return BinaryOpAdd
	case token.SUB:
		return BinaryOpSub
	case token.MUL:
		return BinaryOpMul
	case token.QUO:
		return BinaryOpQuo
	case token.REM:
		return BinaryOpRem
	case token.EQL:
		return BinaryOpEq
	case token.NEQ:
		return BinaryOpNeq
	case token.LSS:
		return BinaryOpLt
	case token.LEQ:
		return BinaryOpLte
	case token.GTR:
		return BinaryOpGt
	case token.GEQ:
		return BinaryOpGte
	case token.AND:
		return BinaryOpAnd
	case token.OR:
		return BinaryOpOr
	case token.XOR:
		return BinaryOpXor
	case token.AND_NOT:
		return BinaryOpAndNot
	case token.LAND:
		return BinaryOpLAnd
	case token.LOR:
		return BinaryOpLOr
	case token.SHL:
		return BinaryOpShl
	case token.SHR:
		return BinaryOpShr
	case token.ARROW:
		return BinaryOpArrow
	default:
		spew.Dump(op)
		panic("unreachable")
	}
}

func ReadUnaryOp(op token.Token) UnaryOp {
	switch op {
	case token.ADD:
		return UnaryOpPos
	case token.SUB:
		return UnaryOpNeg
	case token.NOT:
		return UnaryOpNot
	case token.AND:
		return UnaryOpAddr
	case token.MUL:
		return UnaryOpDeref
	case token.ARROW:
		return UnaryOpArrow
	case token.XOR:
		return UnaryOpBitNot
	default:
		spew.Dump(op)
		panic("unreachable")
	}
}

func ReadCallExpr(expr *ast.CallExpr) Expr {
	var args []Expr
	for _, arg := range expr.Args {
		args = append(args, ReadExpr(arg))
	}
	return &CallExpr{
		Func: ReadExpr(expr.Fun),
		Args: args,
	}
}

func ReadNameExpr(expr *ast.Ident) Expr {
	return &NameExpr{Name: NewIdentifier(expr.Name)}
}

func ReadCompositeLit(expr *ast.CompositeLit) Expr {
	return &CompositeLitExpr{
		Type:  ReadType(expr.Type),
		Elems: ReadCompositeLitElems(expr.Elts),
	}
}

func ReadCompositeLitElems(exprs []ast.Expr) []CompositeLitElem {
	var elems []CompositeLitElem
	for _, expr := range exprs {
		switch expr := expr.(type) {
		case *ast.KeyValueExpr:
			elems = append(elems, CompositeLitElem{
				Key:   ReadExpr(expr.Key),
				Value: ReadExpr(expr.Value),
			})
		default:
			elems = append(elems, CompositeLitElem{
				Key:   nil,
				Value: ReadExpr(expr),
			})
		}
	}
	return elems
}

func ReadLiteral(lit *ast.BasicLit) Literal {
	switch lit.Kind {
	case token.INT:
		return &LiteralInt{Value: lit.Value}
	case token.STRING:
		return &LiteralString{Value: lit.Value}
	case token.CHAR:
		return &LiteralRune{Value: lit.Value}
	case token.FLOAT:
		return &LiteralFloat{Value: lit.Value}
	case token.IMAG:
		return &LiteralImag{Value: lit.Value}
	default:
		spew.Dump(lit)
		panic("unreachable")
	}
}

func ReadType(expr ast.Expr) Type {
	if expr == nil {
		return nil
	}
	switch expr := expr.(type) {
	case *ast.Ident:
		return &TypeName{Name: NewIdentifier(expr.Name)}
	case *ast.ArrayType:
		if expr.Len != nil {
			return &ArrayType{
				ElemType: ReadType(expr.Elt),
				Len:      ReadExpr(expr.Len),
			}
		} else {
			return &SliceType{ElemType: ReadType(expr.Elt)}
		}
	case *ast.StructType:
		fields := []FieldDecl{}
		for _, field := range expr.Fields.List {
			for _, name := range field.Names {
				fields = append(fields, FieldDecl{
					Name: NewIdentifier(name.Name),
					Type: ReadType(field.Type),
				})
			}
		}
		return &StructType{Fields: fields}
	case *ast.IndexExpr:
		return &TypeApplication{
			ID:   ReadQualIdentifier(expr.X),
			Args: []Type{ReadType(expr.Index)},
		}
	case *ast.IndexListExpr:
		return &TypeApplication{
			ID:   ReadQualIdentifier(expr.X),
			Args: ReadTypeList(expr.Indices),
		}
	case *ast.StarExpr:
		return &PointerType{BaseType: ReadType(expr.X)}
	case *ast.InterfaceType:
		return ReadInterfaceType(expr)
	case *ast.FuncType:
		return &FunctionType{
			Signature: ReadSignature(expr),
		}
	case *ast.MapType:
		return &MapType{
			KeyType:  ReadType(expr.Key),
			ElemType: ReadType(expr.Value),
		}
	case *ast.ChanType:
		return &ChannelType{
			Dir:      ReadChanDir(expr.Dir),
			ElemType: ReadType(expr.Value),
		}
	case *ast.SelectorExpr:
		return &QualIdentifier{
			Package: expr.X.(*ast.Ident).Name,
			Name:    NewIdentifier(expr.Sel.Name),
		}
	case *ast.ParenExpr:
		return ReadType(expr.X)
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func ReadQualIdentifier(expr ast.Expr) QualIdentifier {
	switch expr := expr.(type) {
	case *ast.Ident:
		return QualIdentifier{
			Package: "",
			Name:    NewIdentifier(expr.Name),
		}
	case *ast.SelectorExpr:
		return QualIdentifier{
			Package: expr.X.(*ast.Ident).Name,
			Name:    NewIdentifier(expr.Sel.Name),
		}
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func ReadTypeList(exprs []ast.Expr) []Type {
	var result []Type
	for _, expr := range exprs {
		result = append(result, ReadType(expr))
	}
	return result
}

func ReadChanDir(dir ast.ChanDir) ChannelDir {
	switch dir {
	case ast.SEND:
		return ChannelDirSend
	case ast.RECV:
		return ChannelDirRecv
	case ast.SEND | ast.RECV:
		return ChannelDirBoth
	default:
		panic("unreachable")
	}
}

func ReadInterfaceType(expr *ast.InterfaceType) *InterfaceType {
	methods := []MethodElem{}
	constraints := []TypeConstraint{}
	for _, field := range expr.Methods.List {
		switch len(field.Names) {
		case 0:
			// embedded or constraint
			constraints = append(constraints, TypeConstraint{
				TypeElem: TypeElem{
					Union: []TypeTerm{
						{
							Type: ReadTypeConstraint(field.Type),
						},
					},
				},
			})
		case 1:
			switch ty := ReadType(field.Type).(type) {
			case *FunctionType:
				methods = append(methods, MethodElem{
					Name: NewIdentifier(field.Names[0].Name),
					Type: ty,
				})
			default:
				panic("unreachable")
			}
		default:
			panic("unreachable")
		}
	}
	return &InterfaceType{Methods: methods, Constraints: constraints}
}

// ========================

func ParseExpr(src string) Expr {
	expr, err := parser.ParseExpr(src)
	if err != nil {
		panic(err)
	}
	return ReadExpr(expr)
}

func ParseType(src string) Type {
	expr, err := parser.ParseExpr(src)
	if err != nil {
		panic(err)
	}
	return ReadType(expr)
}

// ========================

func main() {
	src := `
package main

func add[T int](x, y T) T {
	return x + y + 3
}

func first[T any](x, y T) T {
	return x
}

func Nil[T any]() T {
	var zero T
	return zero
}

func firstIndex[T any](x []T) T {
	return x[Nil()]
}

func cast[T U, U any](x T) U {
	return x
}

type Hello[T string|int] struct{
	Value T
}

type HelloInt struct{
	Value Hello[int]
}

type TwoHello[T string] struct{
	Value1 Hello[T]
	Value2 Hello[int]
}

type Vec[T any] struct{}

func MakeVec[T any]() Vec[T] {}

func Append[T any](v Vec[T], x T) {}

func ReadVec[T any](v Vec[T]) T {}

func Ptr[T any](x T) *T {
	return &x
}

func useVec() {
	v := MakeVec()
	Append(v, Nil())
	Append(v, Ptr(32))
}

func alloc[T any]() *T {
	return new(T)
}

func hello[T int]() []int {
	return make([]T)
}

func inferStructField() int {
	v := MakeVec()
	Append(v, Nil())
	return ReadVec(v)
}

type F = interface{F()[]int}

func hello[T F](t T) []int {
	return t.F()
}

func example1[U any](t struct{F U}) U {
	return t.F
}

type Pair[T, U any] struct{
	First T
	Second U
}

func makePair[T, U any](x T, y U) Pair[T, U] {
	return Pair[T, U]{First: x, Second: y}
}

type TyEq[T, U any] interface{
	Apply(T) U
}

type TyEqImpl[T any] struct{}
func (_ TyEqImpl[T]) Apply(x T) T {
	return x
}

func Refl[T any]() TyEq[T, T] {
	return TyEqImpl[T]{}
}

type Greeter interface{
	Greet()
}

type Person struct{}
func (*Person) Greet() {}

func MakeGreeter() Greeter {
	return &Person{}
}

type Maker[T any] interface{
	Make() T
}

type IntMaker struct{}
func (IntMaker) Make() int { return 0 }

func UseIntMaker() Maker[int] {
	return &IntMaker{}
}

func PointerThing[T any, U *T|int](t T) U {
	var empty U
	return empty
}

type Receiver struct{}
func (*Receiver) Run() {}

type Runner interface{
	Run()
}

func PointerReceiverThing[T any, U interface{*T; Runner}](t T) {
	var u U = &t
	u.Run()
}

func UsePointerReceiverThing() {
	PointerReceiverThing(Receiver{})
}

type X struct {}
func (x *X) M() int { return 42 }

func CallX_M(x X) int {
	return x.M()
}

func CallHasM[T interface{*X; M() R}, R any](t T) R {
	return t.M()
}

func getValue(id *struct{Value int}) int {
	return id.Value
}

func variadic(xs ...int) []int {
	for _, x := range xs {

	}
	return xs
}

func useVariadic() {
	variadic(1, 2, 3)
	variadic([]int{1,2,3}...)
}

func parse() (int, error) {
	return 0, nil
}

func useParse() bool {
	i, err := parse()
	return i == 0 && err == nil
}

func callParse() (int, error) {
	return parse()
}
`

	_ = src

	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "example.go", nil, parser.ParseComments)
	if err != nil {
		panic(err)
	}

	file := ReadAST("example.go", f)

	c := NewChecker()
	c.LoadFile(file)
	c.CheckFile(file)
}
