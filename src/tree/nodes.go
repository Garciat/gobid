package tree

import (
	"fmt"
	"github.com/davecgh/go-spew/spew"
	. "github.com/garciat/gobid/common"
)

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

type Node interface {
	_Node()
}

type NodeBase struct{}

func (NodeBase) _Node() {}

// ========================

type Expr interface {
	Node
	_Expr()
}

type ExprBase struct {
	NodeBase
}

func (ExprBase) _Expr() {}

type EllipsisExpr struct {
	ExprBase
}

func (EllipsisExpr) String() string {
	return "..."
}

type CallEllipsisExpr struct {
	ExprBase
	Element Expr
}

func (e CallEllipsisExpr) String() string {
	return fmt.Sprintf("...%v", e.Element)
}

type ConstExpr interface {
	Expr
	Type() Type
}

type ConstExprBase struct {
	ExprBase
}

type ConstIntExpr struct {
	ConstExprBase
	Value int64
}

func (*ConstIntExpr) Type() Type {
	return UntypedConstantIntType
}

func (e *ConstIntExpr) String() string {
	return fmt.Sprintf("const int(%d)", e.Value)
}

type ConstUintExpr struct {
	ConstExprBase
	Value uint64
}

func (*ConstUintExpr) Type() Type {
	return UntypedConstantIntType
}

func (e *ConstUintExpr) String() string {
	return fmt.Sprintf("const uint(%d)", e.Value)
}

type ConstBoolExpr struct {
	ConstExprBase
	Value bool
}

func (*ConstBoolExpr) Type() Type {
	return UntypedConstantBoolType
}

func (e *ConstBoolExpr) String() string {
	return fmt.Sprintf("const bool(%v)", e.Value)
}

type ConstStringExpr struct {
	ConstExprBase
	Value string
}

func (*ConstStringExpr) Type() Type {
	return UntypedConstantStringType
}

func (e *ConstStringExpr) String() string {
	return fmt.Sprintf(`const string(%q)`, e.Value)
}

type ConstFloatExpr struct {
	ConstExprBase
	Value float64
}

func (e *ConstFloatExpr) Type() Type {
	return UntypedConstantFloatType
}

func (e *ConstFloatExpr) String() string {
	return fmt.Sprintf("const float(%f)", e.Value)
}

type ConstRuneExpr struct {
	ConstExprBase
	Value rune
}

func (*ConstRuneExpr) Type() Type {
	return UntypedConstantRuneType
}

func (e *ConstRuneExpr) String() string {
	return fmt.Sprintf("const rune(%q)", e.Value)
}

type ConstImagExpr struct {
	ConstExprBase
	Value complex128
}

func (*ConstImagExpr) Type() Type {
	return BuiltinTypeComplex128
}

func (e *ConstImagExpr) String() string {
	return fmt.Sprintf("const imag(%f)", e.Value)
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

type TypeSwitchAssertionExpr struct {
	ExprBase
	Expr Expr
}

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

type ImportRef struct {
	ImportPath         ImportPath
	ImportDeclaredName Identifier
}

func (i ImportRef) _Node() {}

func (i ImportRef) _Expr() {}

func (i ImportRef) _Type() {}

var _ Expr = &ImportRef{}
var _ Type = &ImportRef{}

type PackageNameExpr struct {
	ExprBase
	Path ImportPath
	Name Identifier
}

func IsIgnoreName(e Expr) bool {
	name, ok := e.(*NameExpr)
	return ok && name.Name == IgnoreIdent
}

type LiteralExpr struct {
	ExprBase
	Literal Literal
}

func (e *LiteralExpr) String() string {
	return fmt.Sprintf("literal(%v)", e.Literal)
}

type FuncLitExpr struct {
	ExprBase
	Signature *Signature
	Body      *StatementList
}

type CompositeLitExpr struct {
	ExprBase
	Type  Type
	Elems []*CompositeLitElem
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

func (l *LiteralInt) String() string {
	return fmt.Sprintf("int(%s)", l.Value)
}

type LiteralBool struct {
	LiteralBase
	Value string
}

func (l *LiteralBool) String() string {
	return fmt.Sprintf("bool(%s)", l.Value)
}

type LiteralString struct {
	LiteralBase
	Value string
}

func (l *LiteralString) String() string {
	return fmt.Sprintf("string(%q)", l.Value)
}

type LiteralFloat struct {
	LiteralBase
	Value string
}

func (l *LiteralFloat) String() string {
	return fmt.Sprintf("float(%s)", l.Value)
}

type LiteralImag struct {
	LiteralBase
	Value string
}

func (l *LiteralImag) String() string {
	return fmt.Sprintf("imag(%s)", l.Value)
}

type LiteralRune struct {
	LiteralBase
	Value string
}

func (l *LiteralRune) String() string {
	return fmt.Sprintf("rune(%q)", l.Value)
}

type TypeExpr struct {
	ExprBase
	Type Type
}

// ========================

type Statement interface {
	_Statement()
}

type StatementBase struct {
	NodeBase
}

func (StatementBase) _Statement() {}

type DeclStmt struct {
	StatementBase
	Decls []Decl
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
	Body *StatementList
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
	Cases []*SelectCase
}

type SelectCase struct {
	Comm Statement
	Body *StatementList
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
	Body       *StatementList
}

type ForStmt struct {
	StatementBase
	Init Statement
	Cond Expr
	Post Statement
	Body *StatementList
}

type TypeSwitchStmt struct {
	StatementBase
	Init   Statement
	Assign Statement
	Body   []*TypeSwitchCase
}

type TypeSwitchCase struct {
	Types []Type
	Body  *StatementList
}

type SwitchStmt struct {
	StatementBase
	Init  Statement
	Tag   Expr
	Cases []*SwitchCase
}

type SwitchCase struct {
	Exprs []Expr
	Body  *StatementList
}

type BlockStmt struct {
	StatementBase
	Body *StatementList
}

type StatementList struct {
	Stmts []Statement
}

// ========================

type Decl interface {
	_Decl()
}

type DeclBase struct {
	NodeBase
}

func (DeclBase) _Decl() {}

type ImportDecl struct {
	DeclBase
	ImportPath ImportPath
	Alias      *Identifier
}

func (d *ImportDecl) EffectiveName() Identifier {
	var name string
	if d.Alias != nil {
		name = d.Alias.Value
	} else {
		// TODO (P0) that's not actually how it works
		name = d.ImportPath.PackageName()
	}
	return NewIdentifier(name)
}

type ConstDecl struct {
	DeclBase
	Name  Identifier
	Type  Type
	Value Expr
	Iota  int
}

type VarDecl struct {
	DeclBase
	Names []Identifier
	Type  Type
	Expr  Expr
}

type TypeDecl struct {
	DeclBase
	Name       Identifier
	TypeParams *TypeParamList
	Type       Type
}

type AliasDecl struct {
	DeclBase
	Name Identifier
	Type Type
}

type FunctionDecl struct {
	DeclBase
	Name      Identifier
	Signature *Signature
	Body      *StatementList
}

type MethodDecl struct {
	DeclBase
	Name      Identifier
	Receiver  FieldDecl
	Signature *Signature
	Body      *StatementList
}

// ========================

type ExprVisitor interface {
	Visit(expr Expr) (w ExprVisitor)
}

type ExprVisitorFunc func(expr Expr)

func (f ExprVisitorFunc) Visit(expr Expr) ExprVisitor {
	f(expr)
	return f
}

func WalkExpr(v ExprVisitor, expr Expr) {
	if expr == nil {
		return
	}
	if v = v.Visit(expr); v == nil {
		return
	}

	switch n := expr.(type) {
	case *EllipsisExpr:
	case *ConstIntExpr:
	case *BinaryExpr:
		WalkExpr(v, n.Left)
		WalkExpr(v, n.Right)
	case *UnaryExpr:
		WalkExpr(v, n.Expr)
	case *StarExpr:
		WalkExpr(v, n.Expr)
	case *AddressExpr:
		WalkExpr(v, n.Expr)
	case *ConversionExpr:
		WalkExpr(v, n.Expr)
	case *SelectorExpr:
		WalkExpr(v, n.Expr)
	case *IndexExpr:
		WalkExpr(v, n.Expr)
		for _, e := range n.Indices {
			WalkExpr(v, e)
		}
	case *SliceExpr:
		WalkExpr(v, n.Expr)
		WalkExpr(v, n.Low)
		WalkExpr(v, n.High)
		WalkExpr(v, n.Max)
	case *TypeSwitchAssertionExpr:
		WalkExpr(v, n.Expr)
	case *TypeAssertionExpr:
		WalkExpr(v, n.Expr)
	case *CallExpr:
		WalkExpr(v, n.Func)
		for _, e := range n.Args {
			WalkExpr(v, e)
		}
	case *NameExpr:
	case *LiteralExpr:
	case *FuncLitExpr:
	case *CompositeLitExpr:
		// TODO hacky special case...
		switch t := n.Type.(type) {
		case *ArrayType:
			WalkExpr(v, t.Len)
		}
		for _, e := range n.Elems {
			WalkExpr(v, e.Key)
			WalkExpr(v, e.Value)
		}
	case *TypeExpr:
	case *ImportRef:
	case *PackageNameExpr:
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

// ========================
