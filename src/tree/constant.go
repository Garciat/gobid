package tree

import (
	"fmt"
	"math"
)

type ConstExpr interface {
	Expr
	Type() *UntypedConstantType
	IsRepresentableAs(t *BuiltinType) bool
}

type ConstExprBase struct {
	ExprBase
}

type ConstIntExpr struct {
	ConstExprBase
	Value int64
}

func (*ConstIntExpr) Type() *UntypedConstantType {
	return UntypedConstantIntType
}

func (e *ConstIntExpr) IsRepresentableAs(t *BuiltinType) bool {
	value := e.Value
	switch t.Tag {
	case BuiltinTypeTagInt:
		return math.MinInt <= value && value <= math.MaxInt
	case BuiltinTypeTagInt8:
		return math.MinInt8 <= value && value <= math.MaxInt8
	case BuiltinTypeTagInt16:
		return math.MinInt16 <= value && value <= math.MaxInt16
	case BuiltinTypeTagInt32:
		return math.MinInt32 <= value && value <= math.MaxInt32
	case BuiltinTypeTagInt64:
		return true
	case BuiltinTypeTagUint:
		return 0 <= value
	case BuiltinTypeTagUint8:
		return 0 <= value && value <= math.MaxUint8
	case BuiltinTypeTagUint16:
		return 0 <= value && value <= math.MaxUint16
	case BuiltinTypeTagUint32:
		return 0 <= value && value <= math.MaxUint32
	case BuiltinTypeTagUint64:
		return 0 <= value
	case BuiltinTypeTagUintptr:
		return 0 <= value
	case BuiltinTypeTagFloat32:
		return true
	case BuiltinTypeTagFloat64:
		return true
	case BuiltinTypeTagComplex64:
		return true
	case BuiltinTypeTagComplex128:
		return true
	default:
		return false
	}
}

func (e *ConstIntExpr) String() string {
	return fmt.Sprintf("const int(%d)", e.Value)
}

type ConstUintExpr struct {
	ConstExprBase
	Value uint64
}

func (*ConstUintExpr) Type() *UntypedConstantType {
	return UntypedConstantIntType
}

func (e *ConstUintExpr) IsRepresentableAs(t *BuiltinType) bool {
	value := e.Value
	switch t.Tag {
	case BuiltinTypeTagInt:
		return value <= math.MaxInt
	case BuiltinTypeTagInt8:
		return value <= math.MaxInt8
	case BuiltinTypeTagInt16:
		return value <= math.MaxInt16
	case BuiltinTypeTagInt32:
		return value <= math.MaxInt32
	case BuiltinTypeTagInt64:
		return value <= math.MaxInt64
	case BuiltinTypeTagUint:
		return value <= math.MaxUint
	case BuiltinTypeTagUint8:
		return value <= math.MaxUint8
	case BuiltinTypeTagUint16:
		return value <= math.MaxUint16
	case BuiltinTypeTagUint32:
		return value <= math.MaxUint32
	case BuiltinTypeTagUint64:
		return true
	case BuiltinTypeTagUintptr:
		return true
	case BuiltinTypeTagFloat32:
		return true
	case BuiltinTypeTagFloat64:
		return true
	case BuiltinTypeTagComplex64:
		return true
	case BuiltinTypeTagComplex128:
		return true
	default:
		return false
	}
}

func (e *ConstUintExpr) String() string {
	return fmt.Sprintf("const uint(%d)", e.Value)
}

type ConstBoolExpr struct {
	ConstExprBase
	Value bool
}

func (*ConstBoolExpr) Type() *UntypedConstantType {
	return UntypedConstantBoolType
}

func (e *ConstBoolExpr) IsRepresentableAs(t *BuiltinType) bool {
	return t.Tag == BuiltinTypeTagBool
}

func (e *ConstBoolExpr) String() string {
	return fmt.Sprintf("const bool(%v)", e.Value)
}

type ConstStringExpr struct {
	ConstExprBase
	Value string
}

func (*ConstStringExpr) Type() *UntypedConstantType {
	return UntypedConstantStringType
}

func (e *ConstStringExpr) IsRepresentableAs(t *BuiltinType) bool {
	return t.Tag == BuiltinTypeTagString
}

func (e *ConstStringExpr) String() string {
	return fmt.Sprintf(`const string(%q)`, e.Value)
}

type ConstFloatExpr struct {
	ConstExprBase
	Value float64
}

func (e *ConstFloatExpr) Type() *UntypedConstantType {
	return UntypedConstantFloatType
}

func (e *ConstFloatExpr) IsRepresentableAs(t *BuiltinType) bool {
	switch t.Tag {
	case BuiltinTypeTagFloat32:
		return math.SmallestNonzeroFloat32 <= e.Value && e.Value <= math.MaxFloat32
	case BuiltinTypeTagFloat64:
		return true
	default:
		return false
	}
}

func (e *ConstFloatExpr) String() string {
	return fmt.Sprintf("const float(%f)", e.Value)
}

type ConstRuneExpr struct {
	ConstExprBase
	Value rune
}

func (*ConstRuneExpr) Type() *UntypedConstantType {
	return UntypedConstantRuneType
}

func (e *ConstRuneExpr) IsRepresentableAs(t *BuiltinType) bool {
	return t.Tag == BuiltinTypeTagRune
}

func (e *ConstRuneExpr) String() string {
	return fmt.Sprintf("const rune(%q)", e.Value)
}

type ConstImagExpr struct {
	ConstExprBase
	Value complex128
}

func (*ConstImagExpr) Type() *UntypedConstantType {
	return UntypedConstantComplexType
}

func (e *ConstImagExpr) IsRepresentableAs(t *BuiltinType) bool {
	switch t.Tag {
	case BuiltinTypeTagComplex64:
		x := real(e.Value)
		y := imag(e.Value)
		return math.SmallestNonzeroFloat32 <= x && x <= math.MaxFloat32 &&
			math.SmallestNonzeroFloat32 <= y && y <= math.MaxFloat32
	case BuiltinTypeTagComplex128:
		return true
	default:
		return false
	}
}

func (e *ConstImagExpr) String() string {
	return fmt.Sprintf("const imag(%f)", e.Value)
}
