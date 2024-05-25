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
	Name Identifier
}

func (t *TypeParam) String() string {
	return fmt.Sprintf("%sₚ", t.Name.Name)
}

type TypeApplication struct {
	TypeBase
	Name Identifier
	Args []Type
}

type ArrayType struct {
	TypeBase
	ElemType Type
	Len      Expr
}

type FunctionType struct {
	TypeBase
	Signature Signature
}

func (t *FunctionType) String() string {
	return fmt.Sprintf("func%v", t.Signature)
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
	Name Identifier
	Type Type
}

type StructType struct {
	TypeBase
	Fields []FieldDecl
	// TODO: embedded fields
}

type FieldDecl struct {
	Name Identifier
	Type Type
}

type PointerType struct {
	TypeBase
	BaseType Type
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
	Name      Identifier
	Signature Signature
}

func (m MethodElem) String() string {
	return fmt.Sprintf("%v%v", m.Name, m.Signature)
}

type SliceType struct {
	TypeBase
	ElemType Type
}

type MapType struct {
	TypeBase
	KeyType   Type
	ValueType Type
}

type ChannelType struct {
	TypeBase
	ElemType Type
	Dir      ChannelDir
}

type GenericType struct {
	TypeBase
	TypeParams TypeParamList
	Type       Type
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
	// TODO: div, mod, etc.
)

// ========================

type Expr interface {
	_Expr()
}

type ExprBase struct{}

func (ExprBase) _Expr() {}

type ExprBinaryExpr struct {
	ExprBase
	Op    BinaryOp
	Left  Expr
	Right Expr
}

// TODO: UnaryExpr

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

type StmtSimpleStmt struct {
	StatementBase
	SimpleStmt SimpleStmt
}

type ReturnStmt struct {
	StatementBase
	Results []Expr
}

type IfStmt struct {
	StatementBase
	Init *SimpleStmt
	Cond Expr
	Body StatementList
	Else *IfStmt // Cond==True for plain else
}

// TODO: ForStmt, SwitchStmt, SelectStmt, DeferStmt, GoStmt, LabeledStmt, BlockStmt

type SimpleStmt interface {
	_SimpleStmt()
}

type SimpleStmtBase struct{}

func (SimpleStmtBase) _SimpleStmt() {}

type ExpressionStmt struct {
	SimpleStmtBase
	Expr Expr
}

type EmptyStmt struct {
	SimpleStmtBase
}

type ShortVarDecl struct {
	SimpleStmtBase
	Vars []ShortVarDeclItem
}

type ShortVarDeclItem struct {
	Name Identifier
	Expr Expr
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
	Constraint TypeConstraint
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
	TyCtx    TypeContext
	VarCtx   VarContext
	Builtins VarContext
	CurFn    *FunctionDecl
	Fresh    int
}

func NewChecker() *Checker {
	builtins := VarContext{Types: map[Identifier]Type{
		NewIdentifier("any"):    EmptyInterface(),
		NewIdentifier("int"):    &TypeBuiltin{Name: Identifier{Name: "int"}},
		NewIdentifier("bool"):   &TypeBuiltin{Name: Identifier{Name: "bool"}},
		NewIdentifier("string"): &TypeBuiltin{Name: Identifier{Name: "string"}},
	}}
	return &Checker{
		TyCtx:    TypeContext{},
		VarCtx:   builtins.Fork(),
		Builtins: builtins,
	}
}

func (c *Checker) FreshTypeName() Identifier {
	c.Fresh++
	return Identifier{Name: fmt.Sprintf("@T%d", c.Fresh)}
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
		TyCtx:    c.TyCtx.Fork(),
		VarCtx:   c.VarCtx.Fork(),
		Builtins: c.Builtins,
		CurFn:    c.CurFn,
	}
}

func (c *Checker) BeginFunctionScope(fn *FunctionDecl) *Checker {
	return &Checker{
		TyCtx:    c.TyCtx.Fork(),
		VarCtx:   c.VarCtx.Fork(),
		Builtins: c.Builtins,
		CurFn:    fn,
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

	scope := c.BeginScope()

	for _, tyParam := range decl.TypeParams.Params {
		sub := &TypeName{Name: tyParam.Name}
		super := &InterfaceType{Methods: nil, Constraints: []TypeConstraint{tyParam.Constraint}}
		scope.TyCtx.AddRelation(RelationSubtype{Sub: sub, Super: super})
		scope.VarCtx.Def(tyParam.Name, &TypeParam{Name: tyParam.Name})
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
	switch ty := ty.(type) {
	case *TypeName:
		under, ok := c.VarCtx.Lookup(ty.Name)
		if !ok {
			panic("undefined type")
		}
		c.CheckTypeDeclType(under)
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
		for i, tyArg := range ty.Args {
			tyParam := gen.TypeParams.Params[i]
			super := &InterfaceType{Methods: nil, Constraints: []TypeConstraint{tyParam.Constraint}}
			c.TyCtx.AddRelation(RelationSubtype{Sub: tyArg, Super: super})
			c.CheckTypeDeclType(tyArg)
		}
	case *StructType:
		for _, field := range ty.Fields {
			c.CheckTypeDeclType(field.Type)
		}
	default:
		spew.Dump(ty)
		panic("TODO")
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
		sub := &TypeName{Name: tyParam.Name}
		super := &InterfaceType{Methods: nil, Constraints: []TypeConstraint{tyParam.Constraint}}
		scope.TyCtx.AddRelation(RelationSubtype{Sub: sub, Super: super})
		scope.VarCtx.Def(tyParam.Name, &TypeParam{Name: tyParam.Name})
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
	case *StmtSimpleStmt:
		c.CheckSimpleStmt(stmt.SimpleStmt)
	case *ReturnStmt:
		c.CheckReturnStmt(stmt)
	case *IfStmt:
		c.CheckIfStmt(stmt)
	default:
		panic("unreachable")
	}
}

func (c *Checker) CheckSimpleStmt(stmt SimpleStmt) {
	switch stmt := stmt.(type) {
	case *ExpressionStmt:
		c.Synth(stmt.Expr) // ???
		panic("TODO")
	case *EmptyStmt:
		// do nothing
	case *ShortVarDecl:
		// TODO
	default:
		panic("unreachable")
	}
}

func (c *Checker) CheckReturnStmt(stmt *ReturnStmt) {
	fn := c.AssertInFunctionScope()
	for i, result := range stmt.Results {
		ty := c.Synth(result)
		c.CheckAssignableTo(ty, fn.Signature.Results.Params[i].Type)
	}
}

func (c *Checker) CheckIfStmt(stmt *IfStmt) {
	scope := c.BeginScope()

	if stmt.Init != nil {
		scope.CheckSimpleStmt(*stmt.Init)
	}

	c.CheckExpr(stmt.Cond, c.Builtin("bool"))
}

// ========================

type TypeSet struct {
	Types    []Type
	Universe bool
}

func (c *Checker) CheckSubst(list TypeParamList, subst Subst) {
	for _, tyParam := range list.Params {
		tySub, ok := subst[tyParam.Name]
		if !ok {
			continue // TODO: is this ok?
		}
		set := c.TypeSet(tyParam.Constraint)
		if set.Universe {
			panic("TODO")
		}
		found := false
		for _, ty := range set.Types {
			found = found || c.Identical(ty, tySub)
		}
		if !found {
			panic(fmt.Sprintf("type parameter %v with constraints %v does not satisfy %v", tyParam.Name, tyParam.Constraint, tySub))
		}
	}
}

func (c *Checker) TypeSet(con TypeConstraint) TypeSet {
	// TODO very incomplete
	var types []Type
	for _, term := range con.TypeElem.Union {
		if term.Tilde {
			panic("TODO")
		}
		types = append(types, term.Type)
	}
	return TypeSet{Types: types, Universe: false}
}

// ========================

func (c *Checker) CheckExpr(expr Expr, ty Type) {
	switch expr := expr.(type) {
	case *ExprBinaryExpr:
		c.CheckBinaryExpr(expr, ty)
	case *ConversionExpr:
		panic("TODO")
	case *SelectorExpr:
		panic("TODO")
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
		panic("unreachable")
	}
}

func (c *Checker) CheckBinaryExpr(expr *ExprBinaryExpr, ty Type) {
	c.CheckExpr(expr.Left, ty)
	c.CheckExpr(expr.Right, ty)
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
	var funcTy *FunctionType
	var typeArgs []Type
	if index, ok := expr.Func.(*IndexExpr); ok {
		if indexTy, ok := c.Synth(index.Expr).(*FunctionType); ok {
			funcTy = indexTy
			for _, arg := range index.Indices {
				typeArgs = append(typeArgs, c.Synth(arg))
			}
		}
	}
	if funcTy == nil {
		var ok bool
		funcTy, ok = c.Synth(expr.Func).(*FunctionType)
		if !ok {
			panic("not a function")
		}
	}
	if len(expr.Args) != len(funcTy.Signature.Params.Params) {
		panic("wrong number of arguments")
	}
	if len(typeArgs) > len(funcTy.Signature.TypeParams.Params) {
		panic("too many type arguments")
	}
	if len(funcTy.Signature.TypeParams.Params) > 0 {

	}
	subst := Subst{}
	for _, tyParam := range funcTy.Signature.TypeParams.Params {
		subst[tyParam.Name] = &TypeParam{Name: c.FreshTypeName()}
	}
	funcTy = c.ApplySubst(funcTy, subst).(*FunctionType)
	for i, tyArg := range typeArgs {
		tyParam := funcTy.Signature.TypeParams.Params[i]
		super := &InterfaceType{Constraints: []TypeConstraint{tyParam.Constraint}}
		c.TyCtx.AddRelation(RelationSubtype{Sub: tyArg, Super: super})
		c.TyCtx.AddEq(tyArg, subst[tyParam.Name])
	}
	for i, arg := range expr.Args {
		c.CheckExpr(arg, funcTy.Signature.Params.Params[i].Type)
	}
	// TODO tuple return
	c.CheckAssignableTo(funcTy.Signature.Results.Params[0].Type, ty)
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
	case *ExprBinaryExpr:
		return c.SynthBinaryExpr(expr)
	case *ConversionExpr:
		panic("TODO")
	case *SelectorExpr:
		panic("TODO")
	case *IndexExpr:
		return c.SynthIndexExpr(expr)
	case *TypeAssertionExpr:
		panic("TODO")
	case *CallExpr:
		panic("TODO")
	case *NameExpr:
		return c.SynthNameExpr(expr)
	case *LiteralExpr:
		return c.SynthLiteralExpr(expr)
	case *TypeAppExpr:
		panic("TODO")
	default:
		panic("unreachable")
	}
}

func (c *Checker) SynthBinaryExpr(expr *ExprBinaryExpr) Type {
	tyLeft := c.Synth(expr.Left)
	tyRight := c.Synth(expr.Right)
	c.TyCtx.AddEq(tyLeft, tyRight)
	return tyLeft
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

func (c *Checker) SynthNameExpr(expr *NameExpr) Type {
	ty, ok := c.VarCtx.Lookup(expr.Name)
	if !ok {
		panic("undefined variable")
	}
	return ty
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
	default:
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
		params[i] = TypeParamDecl{
			Name:       param.Name,
			Constraint: c.ApplySubstTypeConstraint(param.Constraint, subst),
		}
	}
	return TypeParamList{Params: params}
}

func (c *Checker) ApplySubstTypeConstraint(constraint TypeConstraint, subst Subst) TypeConstraint {
	return TypeConstraint{TypeElem: c.ApplySubstTypeElem(constraint.TypeElem, subst)}
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

func (c *Checker) Verify() Subst {
	subst := Subst{}

	fmt.Println(c.TyCtx)

	c.Unify(c.TyCtx.Relations, subst)

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

	paramSupers := map[TypeParam][]*InterfaceType{}
	for _, rel := range rels {
		switch rel := rel.(type) {
		case RelationSubtype:
			tyParam, ok := c.Resolve(rel.Sub).(*TypeParam)
			if !ok {
				continue
			}
			superInter, ok := c.Resolve(rel.Super).(*InterfaceType)
			if !ok {
				continue
			}
			paramSupers[*tyParam] = append(paramSupers[*tyParam], superInter)
		}
	}

	for tyParam, supers := range paramSupers {
		if len(supers) > 1 {
			inter := &InterfaceType{Methods: nil, Constraints: nil}
			for _, super := range supers {
				inter.Methods = append(inter.Methods, super.Methods...)
				inter.Constraints = append(inter.Constraints, super.Constraints...)
			}
			c.UnifySubtype(&tyParam, inter, subst)
		}
	}
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
			panic("TODO")
		}
		typeset := c.InterfaceTypeSet(super)
		if typeset.Universe {
			panic("TODO") // check methods
		}
		if len(typeset.Types) == 0 {
			panic("cannot satisfy empty set")
		}
		for _, term := range typeset.Types {
			if !c.IsConcreteType(term) {
				panic("cannot make union of non-concrete types")
			}
			if c.Identical(sub, term) {
				c.UnifyEq(sub, term, subst) // necessary?
				return
			}
		}
		if c.IsTypeParam(sub) {
			return // nothing to add?
		}
		spew.Dump(sub, super)
		panic("TODO")
	case *TypeName:
		superTy, ok := c.VarCtx.Lookup(super.Name)
		if !ok {
			panic("undefined type")
		}
		c.UnifySubtype(sub, superTy, subst)
	case *TypeParam:
		if subTy, ok := sub.(*TypeParam); ok {
			if subTy.Name == super.Name {
				return
			}
		}
	default:
		panic(fmt.Sprintf("TODO: %T", super))
	}
}

func (c *Checker) Resolve(ty Type) Type {
	switch ty.(type) {
	case *TypeName:
		under, ok := c.VarCtx.Lookup(ty.(*TypeName).Name)
		if !ok {
			panic("undefined type")
		}
		return c.Resolve(under)
	default:
		return ty
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
	switch ty := ty.(type) {
	case *TypeBuiltin:
		return true
	case *InterfaceType:
		return false
	case *TypeName:
		under, ok := c.VarCtx.Lookup(ty.Name)
		if !ok {
			panic("undefined type")
		}
		return c.IsConcreteType(under)
	case *TypeParam:
		under, ok := c.VarCtx.Lookup(ty.Name)
		if !ok {
			panic("undefined type") // maybe unbound?
		}
		if s, ok := under.(*TypeParam); ok {
			if s.Name == ty.Name {
				return false
			}
		}
		return c.IsConcreteType(under)
	default:
		panic(fmt.Sprintf("TODO: %T", ty))
	}
}

func (c *Checker) Identical(ty1, ty2 Type) bool {
	// TODO recursive types?
	switch ty1 := ty1.(type) {
	case *TypeName:
		if ty2, ok := ty2.(*TypeName); ok {
			if ty1.Name == ty2.Name {
				return true
			}
		}
		ty, ok := c.VarCtx.Lookup(ty1.Name)
		if !ok {
			panic("undefined type")
		}
		return c.Identical(ty2, ty)
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
	default:
		spew.Dump(ty1, ty2)
		panic("TODO")
	}
}

func (c *Checker) InterfaceTypeSet(ty *InterfaceType) TypeSet {
	// TODO: reference other interfaces?
	if len(ty.Methods) != 0 {
		return TypeSet{Universe: true}
	}
	if len(ty.Constraints) == 0 {
		return TypeSet{Types: []Type{EmptyInterface()}}
	}
	if len(ty.Constraints) == 1 {
		return c.TypeSet(ty.Constraints[0])
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
	return TypeSet{Types: intersection}
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
	default:
		panic("unreachable")
	}
}

func ReadConstDecl(decl *ast.GenDecl) []Decl {
	var decls []Decl
	for _, spec := range decl.Specs {
		spec := spec.(*ast.ValueSpec)
		for i, name := range spec.Names {
			decls = append(decls, &ConstDecl{
				Name:  NewIdentifier(name.Name),
				Type:  ReadType(spec.Type),
				Value: ReadExpr(spec.Values[i]),
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
		params = append(params, TypeParamDecl{
			Name:       NewIdentifier(field.Names[0].Name),
			Constraint: ReadTypeConstraint(field.Type),
		})
	}
	return TypeParamList{Params: params}
}

func ReadTypeConstraint(expr ast.Expr) TypeConstraint {
	return TypeConstraint{TypeElem: ReadTypeElem(expr)}
}

func ReadTypeElem(expr ast.Expr) TypeElem {
	switch expr := expr.(type) {
	case *ast.Ident:
		return TypeElem{Union: []TypeTerm{{Type: &TypeName{Name: NewIdentifier(expr.Name)}}}}
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
		return TypeElem{Union: terms}
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
	var params []ParameterDecl
	for _, field := range list.List {
		for _, name := range field.Names {
			params = append(params, ParameterDecl{
				Name: NewIdentifier(name.Name),
				Type: ReadType(field.Type),
			})
		}
	}
	return ParameterList{Params: params}
}

func ReadResultsList(list *ast.FieldList) ParameterList {
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
	default:
		panic("unreachable")
	}
}

func ReadDeclStmt(stmt *ast.DeclStmt) Statement {
	return &DeclStmt{Decl: ReadDecl(stmt.Decl)[0]}
}

func ReadExprStmt(stmt *ast.ExprStmt) Statement {
	return &StmtSimpleStmt{SimpleStmt: &ExpressionStmt{Expr: ReadExpr(stmt.X)}}
}

func ReadReturnStmt(stmt *ast.ReturnStmt) Statement {
	var results []Expr
	for _, result := range stmt.Results {
		results = append(results, ReadExpr(result))
	}
	return &ReturnStmt{Results: results}
}

func ReadIfStmt(stmt *ast.IfStmt) Statement {
	var init *SimpleStmt
	if stmt.Init != nil {
		init = Ptr(ReadSimpleStmt(stmt.Init))
	}
	if stmt.Else != nil {
		panic("TODO")
	}
	return &IfStmt{
		Init: init,
		Cond: ReadExpr(stmt.Cond),
		Body: ReadBlockStmt(stmt.Body),
		Else: nil, // TODO
	}
}

func ReadSimpleStmt(stmt ast.Stmt) SimpleStmt {
	switch stmt := stmt.(type) {
	case *ast.ExprStmt:
		return &ExpressionStmt{Expr: ReadExpr(stmt.X)}
	case *ast.AssignStmt:
		panic("TODO")
	default:
		panic("unreachable")
	}
}

func ReadExpr(expr ast.Expr) Expr {
	switch expr := expr.(type) {
	case *ast.BinaryExpr:
		return ReadBinaryExpr(expr)
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
	default:
		panic("unreachable")
	}
}

func ReadExprList(exprs []ast.Expr) []Expr {
	var result []Expr
	for _, expr := range exprs {
		result = append(result, ReadExpr(expr))
	}
	return result
}

func ReadBinaryExpr(expr *ast.BinaryExpr) Expr {
	return &ExprBinaryExpr{
		Op:    ReadBinaryOp(expr.Op),
		Left:  ReadExpr(expr.X),
		Right: ReadExpr(expr.Y),
	}
}

func ReadBinaryOp(op token.Token) BinaryOp {
	switch op {
	case token.ADD:
		return BinaryOpAdd
	default:
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

// TODO should break
type TwoHello[T string|int|bool] struct{
	Value1 Hello[T]
	Value2 Hello[T]
}
`

	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "src.go", src, parser.ParseComments)
	if err != nil {
		panic(err)
	}

	// spew.Dump(f)

	file := ReadAST(f)

	// spew.Dump(file)

	c := NewChecker()
	c.CheckFile(file)
}
