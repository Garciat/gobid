package tree

import (
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

type ConstIntExpr struct {
	ExprBase
	Value int
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

func IsIgnoreName(e Expr) bool {
	name, ok := e.(*NameExpr)
	return ok && name.Name == IgnoreIdent
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

type StatementBase struct {
	NodeBase
}

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
	Tag   Expr
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
		name = d.ImportPath.PackageName()
	}
	return NewIdentifier(name)
}

type ConstDecl struct {
	DeclBase
	Elems []ConstDeclElem
}

type ConstDeclElem struct {
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
	Names []Identifier
	Type  Type
	Exprs []Expr
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
		for _, e := range n.Elems {
			WalkExpr(v, e.Key)
			WalkExpr(v, e.Value)
		}
	case *TypeExpr:
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

// ========================
