package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"slices"
	"strings"

	"github.com/davecgh/go-spew/spew"
)

// ========================

func Ptr[T any](x T) *T {
	return &x
}

// ========================

var IgnoreIdent = Identifier{Name: "_"}

type Identifier struct {
	Name string
}

func (i Identifier) String() string {
	return i.Name
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

type TypeBuiltin struct {
	TypeBase
	Name Identifier
}

func (t *TypeBuiltin) String() string {
	return fmt.Sprintf("%sᵢ", t.Name.Name)
}

type TypeName struct {
	TypeBase
	Name Identifier
}

func (t *TypeName) String() string {
	return fmt.Sprintf("%sₙ", t.Name.Name)
}

type TypeParam struct {
	TypeBase
	Name  Identifier
	Bound *InterfaceType
}

func (t *TypeParam) String() string {
	return fmt.Sprintf("%sₚ", t.Name.Name)
}

type TypeApplication struct {
	TypeBase
	Name Identifier
	Args []Type
}

func (t *TypeApplication) String() string {
	parts := []string{}
	for _, arg := range t.Args {
		parts = append(parts, fmt.Sprintf("%v", arg))
	}
	return fmt.Sprintf("%s[%s]", t.Name.Name, strings.Join(parts, ", "))
}

type ArrayType struct {
	TypeBase
	ElemType Type
	Len      Expr
}

func (t *ArrayType) String() string {
	return fmt.Sprintf("[%v]%v", t.Len, t.ElemType)
}

type BuiltinMakeType struct {
	TypeBase
}

func (t *BuiltinMakeType) String() string {
	return "make()"
}

type BuiltinNewType struct {
	TypeBase
}

func (t *BuiltinNewType) String() string {
	return "new()"
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
	// TODO: variadic
}

func (s Signature) String() string {
	return fmt.Sprintf("[%v](%v) (%v)", s.TypeParams, s.Params, s.Results)
}

type ParameterList struct {
	Params []ParameterDecl
}

func (p ParameterList) String() string {
	parts := []string{}
	for _, param := range p.Params {
		parts = append(parts, fmt.Sprintf("%v %v", param.Name, param.Type))
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
	BaseType Type
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

func (t *InterfaceType) IsEmpty() bool {
	return len(t.Methods) == 0 && len(t.Constraints) == 0
}

func EmptyInterface() Type {
	return &InterfaceType{Methods: nil, Constraints: nil}
}

type MethodElem struct {
	Name Identifier
	Type *FunctionType
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

	BinaryOpEq
	BinaryOpNeq
	BinaryOpLt
	BinaryOpLte
	BinaryOpGt
	BinaryOpGte

	BinaryOpAnd
	BinaryOpOr

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
	UnaryOpBitNot

	UnaryOpAddr
	UnaryOpDeref

	UnaryOpArrow
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
	Key   *Identifier
	Value Expr
}

type TypeAppExpr struct {
	ExprBase
	TypeApp TypeApplication
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
	Init Statement
	Tag  Expr // ???
	Body []SwitchCase
}

type SwitchCase struct {
	Exprs []Expr
	Body  StatementList
}

type ForStmt struct {
	StatementBase
	Init Statement
	Cond Expr
	Post Statement
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

type ConstDecl struct {
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
	Methods    []MethodDecl
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
	Receiver  Type
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

type VarContext struct {
	Parent *VarContext
	Types  map[Identifier]Type
}

func (c *VarContext) Def(name Identifier, ty Type) {
	c.Types[name] = ty
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

type Checker struct {
	Fresh *int

	TyCtx    TypeContext
	VarCtx   VarContext
	Builtins VarContext

	// this seems unprincipled
	CurFn *FunctionDecl
	CurTy *TypeDecl
}

func NewChecker() *Checker {
	builtins := VarContext{Types: map[Identifier]Type{
		NewIdentifier("any"): EmptyInterface(),

		NewIdentifier("int"):    &TypeBuiltin{Name: Identifier{Name: "int"}},
		NewIdentifier("bool"):   &TypeBuiltin{Name: Identifier{Name: "bool"}},
		NewIdentifier("string"): &TypeBuiltin{Name: Identifier{Name: "string"}},

		NewIdentifier("make"): &BuiltinMakeType{},
		NewIdentifier("new"):  &BuiltinNewType{},
	}}
	return &Checker{
		Fresh:    Ptr(0),
		TyCtx:    TypeContext{},
		VarCtx:   builtins.Fork(),
		Builtins: builtins,
	}
}

func (c *Checker) FreshTypeName() Identifier {
	*c.Fresh = *c.Fresh + 1
	return Identifier{Name: fmt.Sprintf("@T%d", *c.Fresh)}
}

func (c *Checker) Builtin(name string) Type {
	ty, ok := c.Builtins.Lookup(NewIdentifier(name))
	if !ok {
		panic("undefined builtin")
	}
	return ty
}

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

func (c *Checker) BeginFunctionScope(fn *FunctionDecl) *Checker {
	return &Checker{
		Fresh:    c.Fresh,
		TyCtx:    c.TyCtx.Fork(),
		VarCtx:   c.VarCtx.Fork(),
		Builtins: c.Builtins,
		CurFn:    fn,
		CurTy:    c.CurTy,
	}
}

func (c *Checker) AssertInFunctionScope() *FunctionDecl {
	if c.CurFn == nil {
		panic("not in function scope")
	}
	return c.CurFn
}

// ========================

func (c *Checker) CheckFile(file File) {
	for _, decl := range file.Decls {
		c.CheckDecl(decl)
	}
}

func (c *Checker) CheckDecl(decl Decl) {
	switch decl := decl.(type) {
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
		panic("TODO")
	default:
		panic("unreachable")
	}
}

func (c *Checker) CheckConstDecl(decl *ConstDecl) {
	c.VarCtx.Def(decl.Name, decl.Type)
}

func (c *Checker) CheckTypeDecl(decl *TypeDecl) {
	fmt.Printf("=== CheckTypeDecl(%v) ===\n", decl.Name)

	scope := c.BeginTypeScope(decl)

	for _, tyParam := range decl.TypeParams.Params {
		ty := &TypeParam{Name: tyParam.Name, Bound: tyParam.Constraint}
		// scope.TyCtx.AddRelation(RelationSubtype{Sub: ty, Super: tyParam.Constraint})
		scope.VarCtx.Def(tyParam.Name, ty)
	}

	scope.CheckTypeDeclType(decl.Type)

	subst := scope.Verify()

	scope.CheckSubst(decl.TypeParams, subst)

	if len(decl.TypeParams.Params) > 0 {
		c.VarCtx.Def(decl.Name, &GenericType{TypeParams: decl.TypeParams, Type: decl.Type})
	} else {
		c.VarCtx.Def(decl.Name, decl.Type)
	}
}

func (c *Checker) CheckTypeDeclType(ty Type) {
	switch ty := c.Resolve(ty).(type) {
	case *TypeBuiltin:
		// nothing to do
	case *TypeParam:
		// nothing to do
	case *TypeApplication:
		under, ok := c.VarCtx.Lookup(ty.Name)
		if !ok {
			panic("undefined type")
		}
		gen, ok := under.(*GenericType)
		if !ok {
			panic("not a generic type")
		}
		if len(gen.TypeParams.Params) != len(ty.Args) {
			panic("wrong number of type arguments")
		}
		subst := Subst{}
		for _, tyParam := range gen.TypeParams.Params {
			subst[tyParam.Name] = &TypeParam{Name: c.FreshTypeName(), Bound: tyParam.Constraint}
		}
		gen = c.ApplySubst(gen, subst).(*GenericType)
		for i, tyArg := range ty.Args {
			tyParam := gen.TypeParams.Params[i]
			c.TyCtx.AddRelation(RelationSubtype{Sub: tyArg, Super: tyParam.Constraint}) // Resolve?
			c.CheckTypeDeclType(tyArg)
		}
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
	default:
		spew.Dump(ty)
		panic("TODO")
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
	c.VarCtx.Def(decl.Name, decl.Type)
}

func (c *Checker) CheckVarDecl(decl *VarDecl) {
	c.VarCtx.Def(decl.Name, decl.Type)
}

func (c *Checker) CheckFunctionDecl(decl *FunctionDecl) {
	fmt.Printf("=== CheckFunctionDecl(%v) ===\n", decl.Name)

	scope := c.BeginFunctionScope(decl)

	for _, tyParam := range decl.Signature.TypeParams.Params {
		ty := &TypeParam{Name: tyParam.Name, Bound: tyParam.Constraint}
		// scope.TyCtx.AddRelation(RelationSubtype{Sub: ty, Super: tyParam.Constraint})
		scope.VarCtx.Def(tyParam.Name, ty)
	}

	for _, param := range decl.Signature.Params.Params {
		scope.VarCtx.Def(param.Name, param.Type)
	}

	for _, result := range decl.Signature.Results.Params {
		scope.VarCtx.Def(result.Name, result.Type)
	}

	scope.CheckStatementList(decl.Body)

	subst := scope.Verify()
	scope.CheckSubst(decl.Signature.TypeParams, subst)

	c.VarCtx.Def(decl.Name, &FunctionType{Signature: decl.Signature})
}

func (c *Checker) CheckStatementList(list StatementList) {
	for _, stmt := range list.Stmts {
		c.CheckStatement(stmt)
	}
}

func (c *Checker) CheckStatement(stmt Statement) {
	switch stmt := stmt.(type) {
	case *DeclStmt:
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
	default:
		spew.Dump(stmt)
		panic("unreachable")
	}
}

func (c *Checker) CheckReturnStmt(stmt *ReturnStmt) {
	fn := c.AssertInFunctionScope()
	if len(stmt.Results) != len(fn.Signature.Results.Params) {
		// TODO: tuple return
		panic("wrong number of return values")
	}
	for i, result := range stmt.Results {
		ty := c.Synth(result)
		c.CheckAssignableTo(ty, fn.Signature.Results.Params[i].Type)
	}
}

func (c *Checker) CheckIfStmt(stmt *IfStmt) {
	panic("TODO")
}

func (c *Checker) CheckShortVarDecl(stmt *ShortVarDecl) {
	if len(stmt.Names) != len(stmt.Exprs) {
		// TODO tuple assignment
		panic("wrong number of expressions")
	}
	for i, name := range stmt.Names {
		ty := c.Synth(stmt.Exprs[i])
		c.VarCtx.Def(name, ty)
	}
}

// ========================

type TypeSet struct {
	Methods  []MethodElem
	Types    []Type
	Universe bool
}

func (c *Checker) CheckSubst(tyParams TypeParamList, subst Subst) {
	for _, tyParam := range tyParams.Params {
		tySub, ok := subst[tyParam.Name]
		if !ok {
			continue // TODO: is this ok?
		}
		if single, ok := IsSingleTypeUnion(tyParam.Constraint); ok {
			if c.Identical(tySub, single) {
				continue
			}
		}
		panic(fmt.Sprintf("type param %v with constraint %v cannot be %v", tyParam.Name, tyParam.Constraint, tySub))
	}
}

// ========================

func (c *Checker) CheckExpr(expr Expr, ty Type) {
	switch expr := expr.(type) {
	case *BinaryExpr:
		c.CheckBinaryExpr(expr, ty)
	case *UnaryExpr:
		panic("TODO")
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
	case *TypeAppExpr:
		panic("TODO")
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func (c *Checker) CheckBinaryExpr(expr *BinaryExpr, ty Type) {
	c.CheckExpr(expr.Left, ty)
	c.CheckExpr(expr.Right, ty)
}

func (c *Checker) CheckSelectorExpr(expr *SelectorExpr, ty Type) {
	spew.Dump(expr, ty)
	panic("TODO")
}

func (c *Checker) CheckIndexExpr(expr *IndexExpr, ty Type) {
	switch exprTy := c.Synth(expr.Expr).(type) {
	case *SliceType:
		if len(expr.Indices) != 1 {
			panic("indexing a slice with multiple indices")
		}
		c.CheckExpr(expr.Indices[0], c.Builtin("int"))
		c.TyCtx.AddEq(ty, exprTy.ElemType)
	default:
		panic("TODO")
	}
}

func (c *Checker) CheckCallExpr(expr *CallExpr, ty Type) {
	callTy := c.Synth(expr)
	c.CheckAssignableTo(callTy, ty)
}

func (c *Checker) CheckNameExpr(expr *NameExpr, ty Type) {
	actualTy, ok := c.VarCtx.Lookup(expr.Name)
	if !ok {
		panic("undefined variable")
	}
	c.TyCtx.AddEq(actualTy, ty)
}

func (c *Checker) CheckLiteralExpr(expr *LiteralExpr, ty Type) {
	switch expr.Literal.(type) {
	case *LiteralInt:
		c.TyCtx.AddEq(ty, c.Builtin("int"))
	case *LiteralBool:
		c.TyCtx.AddEq(ty, c.Builtin("bool"))
	case *LiteralString:
		c.TyCtx.AddEq(ty, c.Builtin("string"))
	default:
		panic("unreachable")
	}
}

func (c *Checker) CheckAssignableTo(src, dst Type) {
	c.TyCtx.AddRelation(RelationSubtype{Sub: src, Super: dst})
}

// ========================

func (c *Checker) Synth(expr Expr) Type {
	switch expr := expr.(type) {
	case *BinaryExpr:
		return c.SynthBinaryExpr(expr)
	case *UnaryExpr:
		return c.SynthUnaryExpr(expr)
	case *ConversionExpr:
		panic("TODO")
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
	case *TypeAppExpr:
		panic("TODO")
	case *TypeExpr:
		return expr.Type
	case *CompositeLitExpr:
		return c.SynthCompositeLitExpr(expr)
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func (c *Checker) SynthBinaryExpr(expr *BinaryExpr) Type {
	tyLeft := c.Synth(expr.Left)
	tyRight := c.Synth(expr.Right)
	c.TyCtx.AddEq(tyLeft, tyRight)
	return tyLeft
}

func (c *Checker) SynthUnaryExpr(expr *UnaryExpr) Type {
	ty := c.Synth(expr.Expr)
	switch expr.Op {
	case UnaryOpAddr:
		return &PointerType{BaseType: ty}
	case UnaryOpDeref:
		switch ty := ty.(type) {
		case *PointerType:
			return ty.BaseType
		default:
			panic("cannot dereference non-pointer")
		}
	default:
		spew.Dump(expr)
		panic("TODO")
	}
}

func (c *Checker) SynthSelectorExpr(expr *SelectorExpr) Type {
	switch ty := c.Synth(expr.Expr).(type) {
	case *StructType:
		for _, field := range ty.Fields {
			if field.Name == expr.Sel {
				return field.Type
			}
		}
		panic("undefined field")
	case *TypeParam:
		if ty.Bound != nil {
			set := c.InterfaceTypeSet(ty.Bound)
			for _, m := range set.Methods {
				if m.Name == expr.Sel {
					return m.Type
				}
			}
		}
		panic(fmt.Sprintf("type %v has no field %v", ty, expr.Sel))
	default:
		panic(fmt.Sprintf("type %v has no field %v", ty, expr.Sel))
	}
}

func (c *Checker) SynthIndexExpr(expr *IndexExpr) Type {
	switch exprTy := c.Synth(expr.Expr).(type) {
	case *SliceType:
		if len(expr.Indices) != 1 {
			panic("indexing a slice with multiple indices")
		}
		c.CheckExpr(expr.Indices[0], c.Builtin("int"))
		return exprTy.ElemType
	case *FunctionType:
		panic("unexpected function type (should be handled by CallExpr)")
	default:
		spew.Dump(exprTy)
		panic("TODO")
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
		case *BuiltinNewType:
			if len(expr.Args) != 1 {
				panic("builtin new() takes exactly one argument")
			}
			return &PointerType{BaseType: c.Synth(expr.Args[0])}
		case *BuiltinMakeType:
			if len(expr.Args) == 0 {
				panic("builtin make() takes at least one argument")
			}
			elemTy := c.Synth(expr.Args[0])
			switch elemTy.(type) {
			case *SliceType:
			case *MapType:
			case *ChannelType:
			default:
				panic("make() with non-slice, non-map, non-channel type")
			}
			for _, arg := range expr.Args[1:] {
				c.CheckExpr(arg, c.Builtin("int"))
			}
			return elemTy
		default:
			panic("not a function")
		}
	}
	if len(expr.Args) != len(funcTy.Signature.Params.Params) {
		panic("wrong number of arguments")
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
		c.TyCtx.AddRelation(RelationSubtype{Sub: tyArg, Super: tyParam.Constraint})
		c.TyCtx.AddEq(tyArg, subst[tyParam.Name])
	}
	for i, arg := range expr.Args {
		c.CheckExpr(arg, funcTy.Signature.Params.Params[i].Type)
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

func (c *Checker) SynthNameExpr(expr *NameExpr) Type {
	ty, ok := c.VarCtx.Lookup(expr.Name)
	if !ok {
		panic("undefined variable")
	}
	return c.Resolve(ty)
}

func (c *Checker) SynthLiteralExpr(expr *LiteralExpr) Type {
	switch expr.Literal.(type) {
	case *LiteralInt:
		return c.Builtin("int")
	case *LiteralBool:
		return c.Builtin("bool")
	case *LiteralString:
		return c.Builtin("string")
	default:
		panic("unreachable")
	}
}

func (c *Checker) SynthCompositeLitExpr(expr *CompositeLitExpr) Type {
	var structTy *StructType

	switch ty := c.Resolve(expr.Type).(type) {
	case *StructType:
		structTy = ty
	case *TypeApplication:
		applied := c.TypeApplication(ty)
		if ty, ok := applied.(*StructType); ok {
			structTy = ty
		}
	}

	if structTy == nil {
		panic("composite literal with non-struct type")
	}

	if len(expr.Elems) == 0 {
		return structTy
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
				if field.Name == *elem.Key {
					c.CheckExpr(elem.Value, field.Type)
					continue elems
				}
			}
			panic(fmt.Sprintf("type %v has no field %v", structTy, elem.Key))
		}
	}

	return structTy
}

func (c *Checker) TypeApplication(app *TypeApplication) Type {
	under, ok := c.VarCtx.Lookup(app.Name)
	if !ok {
		panic("undefined type")
	}
	gen, ok := under.(*GenericType)
	if !ok {
		panic("not a generic type")
	}
	if len(gen.TypeParams.Params) != len(app.Args) {
		panic("wrong number of type arguments")
	}
	rename := Subst{}
	for _, tyParam := range gen.TypeParams.Params {
		rename[tyParam.Name] = &TypeParam{Name: c.FreshTypeName(), Bound: tyParam.Constraint}
	}
	gen = c.ApplySubst(gen, rename).(*GenericType)
	subst := Subst{}
	for i, tyArg := range app.Args {
		tyParam := gen.TypeParams.Params[i]
		c.TyCtx.AddRelation(RelationSubtype{Sub: tyArg, Super: tyParam.Constraint}) // Resolve?
		subst[tyParam.Name] = tyArg
	}
	return c.ApplySubst(gen.Type, subst)
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
		return &TypeApplication{Name: ty.Name, Args: args}
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
		methods := make([]MethodElem, len(ty.Methods))
		for i, method := range ty.Methods {
			methods[i] = MethodElem{
				Name: method.Name,
				Type: c.ApplySubst(method.Type, subst).(*FunctionType),
			}
		}
		constraints := make([]TypeConstraint, len(ty.Constraints))
		for i, constraint := range ty.Constraints {
			constraints[i] = TypeConstraint{TypeElem: c.ApplySubstTypeElem(constraint.TypeElem, subst)}
		}
		return &InterfaceType{Methods: methods, Constraints: constraints}
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
	default:
		spew.Dump(ty)
		panic("TODO")
	}
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
			Name: param.Name,
			Type: c.ApplySubst(param.Type, subst),
		}
	}
	return ParameterList{Params: params}
}

// ========================

func (c *Checker) Simplify(subst Subst) {
	for k, v := range subst {
		cur := v
		for {
			if param, ok := cur.(*TypeParam); ok {
				if k.Name == param.Name.Name {
					panic("circular substitution?")
				}
				if next, ok := subst[param.Name]; ok {
					cur = next
					continue
				} else {
					break
				}
			}
			break
		}
		subst[k] = cur
	}
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
	fmt.Println(c.TyCtx)

	rels := c.TyCtx.Relations

	for i := 0; i < 10; i++ {
		fmt.Printf("=== iteration %d ===\n", i)
		fmt.Println(rels)

		learned := Subst{}

		c.Unify(rels, learned)
		c.Simplify(learned)

		fmt.Printf("learned: %v\n", learned)

		next := []Relation{}

		for _, rel := range rels {
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
			default:
				panic("unreachable")
			}
		}

		rels = next
		subst = c.Merge(subst, learned)

		if len(learned) == 0 {
			break
		}
	}

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
		default:
			panic("unreachable")
		}
	}

	// TODO is there a principled way to do this?

	// params := map[Identifier]*TypeParam{}
	// paramSupers := map[Identifier][]*InterfaceType{}
	// for _, rel := range rels {
	// 	switch rel := rel.(type) {
	// 	case RelationSubtype:
	// 		tyParam, ok := c.Resolve(rel.Sub).(*TypeParam)
	// 		if !ok {
	// 			continue
	// 		}
	// 		superInter, ok := c.Resolve(rel.Super).(*InterfaceType)
	// 		if !ok {
	// 			continue
	// 		}
	// 		params[tyParam.Name] = tyParam
	// 		paramSupers[tyParam.Name] = append(paramSupers[tyParam.Name], superInter)
	// 	}
	// }

	// for tyParam, supers := range paramSupers {
	// 	if len(supers) > 1 {
	// 		inter := IntersectInterfaces(supers...)
	// 		c.UnifySubtype(params[tyParam], inter, subst)
	// 	}
	// }
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
	fmt.Printf("? %v = %v %v\n", left, right, subst)

	if c.Identical(left, right) {
		return
	}

	switch left := left.(type) {
	case *TypeName:
		leftTy, ok := c.VarCtx.Lookup(left.Name)
		if !ok {
			panic("undefined type")
		}
		c.UnifyEq(right, leftTy, subst)
	case *TypeBuiltin:
		if _, ok := right.(*TypeBuiltin); ok {
			panic(fmt.Sprintf("cannot unify: %v = %v", left, right))
		}
		c.UnifyEq(right, left, subst)
	case *TypeParam:
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
		panic("TODO")
	case *TypeApplication:
		if right, ok := right.(*TypeApplication); ok {
			if left.Name != right.Name {
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
	default:
		spew.Dump(left, right)
		panic("TODO")
	}
}

func (c *Checker) UnifySubtype(sub, super Type, subst Subst) {
	fmt.Printf("? %v <: %v %v\n", sub, super, subst)

	if c.IsConcreteType(super) {
		c.UnifyEq(sub, super, subst)
		return
	}

	if sub, ok := c.Resolve(sub).(*TypeParam); ok {
		if !c.Identical(sub, super) && c.ContainsTypeParam(super, sub) {
			panic(fmt.Sprintf("circular constraint: %v <: %v", sub, super))
		}
	}
	if super, ok := c.Resolve(super).(*TypeParam); ok {
		if !c.Identical(sub, super) && c.ContainsTypeParam(sub, super) {
			panic(fmt.Sprintf("circular constraint: %v <: %v", sub, super))
		}
	}

	// special cases???
	switch sub := c.Resolve(sub).(type) {
	case *PointerType:
		c.UnifyEq(sub, super, subst)
		return
	}

	switch super := super.(type) {
	case *InterfaceType:
		// if sub, ok := c.Resolve(sub).(*TypeParam); ok {
		// 	for _, rel := range c.TyCtx.Relations {

		// 		switch rel := rel.(type) {
		// 		case RelationEq:
		// 			if c.Identical(sub, rel.Left) {
		// 				c.UnifySubtype(rel.Right, super, subst)
		// 			}
		// 			if c.Identical(sub, rel.Right) {
		// 				c.UnifySubtype(rel.Left, super, subst)
		// 			}
		// 		case RelationSubtype:
		// 			if c.Identical(sub, rel.Super) {
		// 				c.UnifySubtype(rel.Sub, super, subst)
		// 			}
		// 			if c.Identical(sub, rel.Sub) {
		// 				c.UnifySubtype(rel.Super, super, subst)
		// 			}
		// 		}
		// 	}
		// 	return
		// }
		super = c.SimplifyInterface(super)
		if super.IsEmpty() {
			return
		}
		if single, ok := IsSingleTypeUnion(super); ok {
			c.UnifySubtype(sub, single, subst)
			return
		}
		if len(super.Methods) > 0 {
			spew.Dump(sub, super)
			panic("TODO")
		}
		supertypeset := c.InterfaceTypeSet(super)
		if supertypeset.Universe {
			panic("TODO") // check methods
		}
		if len(supertypeset.Types) == 0 {
			panic("cannot satisfy empty set")
		}
		if tyPar, ok := c.Resolve(sub).(*TypeParam); ok {

			var bound Type = tyPar.Bound
			if tyPar.Bound == nil {
				bound = EmptyInterface()
			}
			c.UnifySubtype(bound, super, subst)
			return
		}
		if sub, ok := c.Resolve(sub).(*InterfaceType); ok {
			if sub.IsEmpty() {
				return
			}
			// TODO check methods
			subtypeset := c.InterfaceTypeSet(sub)
			if subtypeset.Universe {
				panic("TODO")
			}
			for _, term := range subtypeset.Types {
				found := false
				for _, superTerm := range supertypeset.Types {
					if c.Identical(term, superTerm) {
						found = true
					}
				}
				if !found {
					panic(fmt.Sprintf("interface %v does not satisfy %v", sub, super))
				}
			}
			return
		}
		for _, term := range supertypeset.Types {
			if !c.IsConcreteType(term) {
				panic("cannot make union of non-concrete types")
			}
			if c.Identical(sub, term) {
				c.UnifyEq(sub, term, subst) // necessary?
				return
			}
		}
		panic(fmt.Sprintf("type %v does not satisfy %v", sub, super))
	case *TypeName:
		superTy, ok := c.VarCtx.Lookup(super.Name)
		if !ok {
			panic("undefined type")
		}
		c.UnifySubtype(sub, superTy, subst)
	case *TypeParam:
		if subTy, ok := c.Resolve(sub).(*TypeParam); ok {
			if subTy.Name == super.Name {
				return
			}
		}
	default:
		spew.Dump(sub, super)
		panic("TODO")
	}
}

func (c *Checker) Resolve(ty Type) Type {
	switch ty := ty.(type) {
	case *TypeName:
		under, ok := c.VarCtx.Lookup(ty.Name)
		if !ok {
			spew.Dump(c.VarCtx)
			panic(fmt.Sprintf("undefined type: %v", ty.Name))
		}
		return c.Resolve(under)
	default:
		return ty
	}
}

func (c *Checker) ContainsTypeParam(ty Type, tyParam *TypeParam) bool {
	switch ty := c.Resolve(ty).(type) {
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

func (c *Checker) IsTypeParam(ty Type) bool {
	switch c.Resolve(ty).(type) {
	case *TypeParam:
		return true
	default:
		return false
	}
}

func (c *Checker) IsConcreteType(ty Type) bool {
	switch ty := c.Resolve(ty).(type) {
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
	default:
		spew.Dump(ty)
		panic("TODO")
	}
}

func (c *Checker) Identical(ty1, ty2 Type) bool {
	// fmt.Printf("== Identical(%v, %v) ==\n", ty1, ty2)
	// TODO recursive types?
	switch ty1 := ty1.(type) {
	case *TypeName:
		if ty2, ok := ty2.(*TypeName); ok {
			if ty1.Name == ty2.Name {
				return true
			}
		}
		return c.Identical(ty2, c.Resolve(ty1))
	case *TypeBuiltin:
		ty, ok := c.Resolve(ty2).(*TypeBuiltin)
		if !ok {
			return false
		}
		return ty1.Name == ty.Name
	case *TypeParam:
		if ty2, ok := c.Resolve(ty2).(*TypeParam); ok {
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
			return c.Identical(ty1.BaseType, ty2.BaseType)
		}
		return false
	case *TypeApplication:
		if ty2, ok := ty2.(*TypeApplication); ok {
			if ty1.Name != ty2.Name {
				return false
			}
			if len(ty1.Args) != len(ty2.Args) {
				return false
			}
			for i, arg := range ty1.Args {
				if !c.Identical(arg, ty2.Args[i]) {
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
			return c.Identical(ty1.ElemType, ty2.ElemType)
		}
		return false
	default:
		spew.Dump(ty1, ty2)
		panic("TODO")
	}
}

func (c *Checker) InterfaceTypeSet(ty *InterfaceType) TypeSet {
	if len(ty.Constraints) == 0 {
		return TypeSet{Methods: ty.Methods}
	}
	methods := []MethodElem{}
	copy(methods, ty.Methods)
	for _, constraint := range ty.Constraints {
		for _, elem := range c.TypeSet(constraint).Methods {
			methods = append(methods, elem)
		}
	}
	intersection := c.TypeSet(ty.Constraints[0]).Types
	for _, constraint := range ty.Constraints[1:] {
		next := []Type{}
		for _, ty := range c.TypeSet(constraint).Types {
			for _, elem := range intersection {
				if c.Identical(ty, elem) {
					next = append(next, ty) // SLOW!?
				}
			}
		}
		intersection = next
	}
	return TypeSet{Types: intersection, Methods: methods}
}

func (c *Checker) TypeSet(con TypeConstraint) TypeSet {
	if len(con.TypeElem.Union) == 1 {
		term := con.TypeElem.Union[0]
		if term.Tilde {
			panic("TODO")
		}
		switch ty := c.Resolve(term.Type).(type) {
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
		switch ty := c.Resolve(term.Type).(type) {
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
			singleRef, ok := c.VarCtx.Lookup(single.Name)
			if !ok {
				panic("undefined type")
			}
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

func ReadAST(file *ast.File) File {
	var decls []Decl
	for _, decl := range file.Decls {
		decls = append(decls, ReadDecl(decl)...)
	}
	return File{Decls: decls}
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
		return nil
	default:
		spew.Dump(decl)
		panic("unreachable")
	}
}

func ReadConstDecl(decl *ast.GenDecl) []Decl {
	// TODO iota?
	var decls []Decl
	for _, spec := range decl.Specs {
		spec := spec.(*ast.ValueSpec)
		for i, name := range spec.Names {
			var value Expr
			if spec.Values != nil {
				value = ReadExpr(spec.Values[i])
			}
			decls = append(decls, &ConstDecl{
				Name:  NewIdentifier(name.Name),
				Type:  ReadType(spec.Type),
				Value: value,
			})
		}
	}
	return decls
}

func ReadTypeDecl(decl *ast.GenDecl) []Decl {
	var decls []Decl
	for _, spec := range decl.Specs {
		spec := spec.(*ast.TypeSpec)
		decls = append(decls, &TypeDecl{
			Name:       NewIdentifier(spec.Name.Name),
			TypeParams: ReadTypeParamList(spec.TypeParams),
			Type:       ReadType(spec.Type),
			Methods:    nil,
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
	return []Decl{&FunctionDecl{
		Name:      NewIdentifier(decl.Name.Name),
		Signature: ReadSignature(decl.Type),
		Body:      ReadBlockStmt(decl.Body),
	}}
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
			terms = append(terms, TypeTerm{Type: ReadType(cur.Y)})
			if next, ok := cur.X.(*ast.BinaryExpr); ok {
				cur = next
			} else {
				terms = append(terms, TypeTerm{Type: ReadType(cur.X)})
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
	case *ast.StructType:
		return &InterfaceType{
			Methods: nil,
			Constraints: []TypeConstraint{
				{
					TypeElem{
						Union: []TypeTerm{
							{Type: ReadType(expr)},
						},
					},
				},
			},
		}
	default:
		spew.Dump(expr)
		panic("unreachable")
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
	for _, field := range list.List {
		for _, name := range field.Names {
			var ty Type
			var variadic bool
			if ellipsis, variadic := field.Type.(*ast.Ellipsis); variadic {
				ty = &SliceType{ElemType: ReadType(ellipsis.Elt)}
				variadic = true
			} else {
				ty = ReadType(field.Type)
			}
			params = append(params, ParameterDecl{
				Name:     NewIdentifier(name.Name),
				Type:     ty,
				Variadic: variadic,
			})
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
		params = append(params, ParameterDecl{
			Name: IgnoreIdent,
			Type: ReadType(field.Type),
		})
	}
	return ParameterList{Params: params}
}

func ReadBlockStmt(block *ast.BlockStmt) StatementList {
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
		Body: ReadBlockStmt(stmt.Body),
		Else: elseStmt, // TODO
	}
}

func ReadElseStmt(stmt ast.Stmt) Statement {
	switch stmt := stmt.(type) {
	case *ast.BlockStmt:
		return &IfStmt{
			Cond: nil,
			Body: ReadBlockStmt(stmt),
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
		Body:   ReadBlockStmt(stmt.Body),
	}
}

func ReadIncDecStmt(stmt *ast.IncDecStmt) Statement {
	return &IncDecStmt{
		Expr: ReadExpr(stmt.X),
		Inc:  stmt.Tok == token.INC,
	}
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
		Init: init,
		Tag:  tag,
		Body: ReadSwitchCases(stmt.Body.List),
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
		Body: ReadBlockStmt(stmt.Body),
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
		Body:      ReadBlockStmt(expr.Body),
	}
}

func ReadBinaryOp(op token.Token) BinaryOp {
	switch op {
	case token.ADD:
		return BinaryOpAdd
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
	case token.LAND:
		return BinaryOpLAnd
	case token.LOR:
		return BinaryOpLOr
	case token.ARROW:
		return BinaryOpArrow
	default:
		spew.Dump(op)
		panic("unreachable")
	}
}

func ReadUnaryOp(op token.Token) UnaryOp {
	switch op {
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
				Key:   Ptr(NewIdentifier(expr.Key.(*ast.Ident).Name)),
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
	default:
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
		}
		if expr.Len == nil {
			return &SliceType{ElemType: ReadType(expr.Elt)}
		}
		panic("TODO")
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
			Name: NewIdentifier(expr.X.(*ast.Ident).Name),
			Args: []Type{ReadType(expr.Index)},
		}
	case *ast.IndexListExpr:
		return &TypeApplication{
			Name: NewIdentifier(expr.X.(*ast.Ident).Name),
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
		// TODO packages :')
		return &TypeName{Name: NewIdentifier(expr.Sel.Name)}
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
	for _, field := range expr.Methods.List {
		switch ty := ReadType(field.Type).(type) {
		case *FunctionType:
			methods = append(methods, MethodElem{
				Name: NewIdentifier(field.Names[0].Name),
				Type: ty,
			})
		default:
			panic("TODO")

		}
	}
	return &InterfaceType{Methods: methods}
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
`

	_ = src

	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "main.go", src, parser.ParseComments)
	if err != nil {
		panic(err)
	}

	// spew.Dump(f)

	file := ReadAST(f)

	// spew.Dump(file)

	c := NewChecker()
	c.CheckFile(file)
}
