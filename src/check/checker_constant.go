package check

import (
	"fmt"
	"github.com/davecgh/go-spew/spew"
	"github.com/garciat/gobid/common"
	"github.com/garciat/gobid/tree"
	"math"
	"strconv"
	"strings"
)

func (c *Checker) EvaluateConstant(ty tree.Type, decl *tree.ConstDecl) tree.Expr {
	switch ty := c.ResolveType(c.Under(ty)).(type) {
	case *tree.TypeBuiltin:
		switch ty.Name.Value {
		case "int", "int8", "int16", "int32", "int64":
			return &tree.ConstIntExpr{Value: c.EvaluateConstantIntExpr(decl, decl.Value)}
		case "uintptr", "uint", "uint8", "uint16", "uint32", "uint64":
			return &tree.ConstUintExpr{Value: c.EvaluateConstantUintExpr(decl, decl.Value)}
		case "bool":
			return &tree.ConstBoolExpr{Value: c.EvaluateConstantBoolExpr(decl, decl.Value)}
		case "string":
			return &tree.ConstStringExpr{Value: c.EvaluateConstantStringExpr(decl, decl.Value)}
		case "float32", "float64":
			return &tree.ConstFloatExpr{Value: c.EvaluateConstantFloatExpr(decl, decl.Value)}
		default:
			spew.Dump(ty)
			panic("unreachable")
		}
	case *tree.UntypedConstantType:
		switch ty.Kind {
		case tree.UntypedConstantInt:
			return &tree.ConstIntExpr{Value: c.EvaluateConstantIntExpr(decl, decl.Value)}
		case tree.UntypedConstantBool:
			return &tree.ConstBoolExpr{Value: c.EvaluateConstantBoolExpr(decl, decl.Value)}
		case tree.UntypedConstantString:
			return &tree.ConstStringExpr{Value: c.EvaluateConstantStringExpr(decl, decl.Value)}
		case tree.UntypedConstantFloat:
			return &tree.ConstFloatExpr{Value: c.EvaluateConstantFloatExpr(decl, decl.Value)}
		case tree.UntypedConstantRune:
			return &tree.ConstRuneExpr{Value: c.EvaluateConstantRuneExpr(decl, decl.Value)}
		default:
			spew.Dump(ty)
			panic("unreachable")
		}
	default:
		spew.Dump(ty)
		panic("unreachable")
	}
}

func (c *Checker) EvaluateConstantExpr(decl *tree.ConstDecl, expr tree.Expr) tree.Expr {
	switch expr := expr.(type) {
	case *tree.NameExpr:
		switch expr.Name.Value {
		case "true":
			return &tree.ConstBoolExpr{Value: true}
		case "false":
			return &tree.ConstBoolExpr{Value: false}
		case "iota":
			if decl == nil {
				panic("iota outside const declaration")
			}
			return &tree.ConstIntExpr{Value: int64(decl.Iota)}
		default:
			return c.EvaluateConstantExpr(decl, c.LookupConst(expr.Name))
		}
	case *tree.SelectorExpr:
		ty, ok := expr.Expr.(*tree.TypeExpr)
		if !ok {
			spew.Dump(expr)
			panic("unreachable?")
		}
		lhs, ok := ty.Type.(*tree.ImportRef)
		if !ok {
			spew.Dump(expr)
			panic("unreachable?")
		}
		return c.EvaluateConstantExpr(decl, c.PackageLookupConst(lhs.ImportPath, expr.Sel))
	case *tree.PackageNameExpr:
		return c.EvaluateConstantExpr(decl, c.PackageLookupConst(expr.Path, expr.Name))
	case *tree.ConstIntExpr:
		return expr
	case *tree.ConstUintExpr:
		return expr
	case *tree.ConstFloatExpr:
		return expr
	case *tree.ConstBoolExpr:
		return expr
	case *tree.ConstStringExpr:
		return expr
	case *tree.ConstRuneExpr:
		return expr
	case *tree.LiteralExpr:
		switch lit := expr.Literal.(type) {
		case *tree.LiteralInt:
			s := strings.Replace(lit.Value, "_", "", -1)
			if strings.HasPrefix(s, "0x") {
				value, err := strconv.ParseUint(s[2:], 16, 64)
				if err != nil {
					panic(err)
				}
				return &tree.ConstIntExpr{Value: int64(value)}
			} else if strings.HasPrefix(s, "0b") {
				value, err := strconv.ParseUint(s[2:], 2, 64)
				if err != nil {
					panic(err)
				}
				return &tree.ConstIntExpr{Value: int64(value)}
			} else if strings.HasPrefix(s, "0") {
				value, err := strconv.ParseUint(s, 8, 64)
				if err != nil {
					panic(err)
				}
				return &tree.ConstIntExpr{Value: int64(value)}
			} else {
				value, err := strconv.ParseUint(s, 10, 64)
				if err != nil {
					panic(err)
				}
				return &tree.ConstIntExpr{Value: int64(value)}
			}
		case *tree.LiteralBool:
			return &tree.ConstBoolExpr{Value: lit.Value == "true"}
		case *tree.LiteralString:
			return &tree.ConstStringExpr{Value: lit.Value}
		case *tree.LiteralFloat:
			value, err := strconv.ParseFloat(lit.Value, 64)
			if err != nil {
				panic(err)
			}
			return &tree.ConstFloatExpr{Value: value}
		case *tree.LiteralRune:
			common.Assert(lit.Value[0] == '\'', "rune literal not single-quoted")
			common.Assert(lit.Value[len(lit.Value)-1] == '\'', "rune literal not single-quoted")

			s := lit.Value[1 : len(lit.Value)-1] // remove single quotes?

			if strings.HasPrefix(s, "\\u") {
				value, err := strconv.ParseUint(s[2:], 16, 64)
				if err != nil {
					panic(err)
				}
				return &tree.ConstRuneExpr{Value: rune(value)}
			} else if strings.HasPrefix(s, "\\U") {
				value, err := strconv.ParseUint(s[2:], 16, 64)
				if err != nil {
					panic(err)
				}
				return &tree.ConstRuneExpr{Value: rune(value)}
			} else if strings.HasPrefix(s, "\\") {
				value, err := strconv.ParseUint(s[1:], 8, 64)
				if err != nil {
					panic(err)
				}
				return &tree.ConstRuneExpr{Value: rune(value)}
			} else {
				return &tree.ConstRuneExpr{Value: rune(s[0])}
			}
		default:
			spew.Dump(lit)
			panic("unreachable")
		}
	case *tree.BinaryExpr:
		left := c.EvaluateConstantExpr(decl, expr.Left)
		right := c.EvaluateConstantExpr(decl, expr.Right)

		//if reflect.TypeOf(left) != reflect.TypeOf(right) {
		//	panic(fmt.Errorf("binary expression with different types %v and %v", left, right))
		//}

		switch expr.Op {
		case tree.BinaryOpEq:
			return &tree.ConstBoolExpr{Value: left == right}
		case tree.BinaryOpNeq:
			return &tree.ConstBoolExpr{Value: left != right}
		}

		switch right := right.(type) {
		case *tree.ConstFloatExpr:
			return EvaluateBinaryOpFloat(expr.Op, c.EvaluateConstantFloatExpr(decl, left), right.Value)
		}

		switch left := left.(type) {
		case *tree.ConstIntExpr:
			return EvaluateBinaryOpInt(expr.Op, left.Value, c.EvaluateConstantIntExpr(decl, right))
		case *tree.ConstUintExpr:
			return EvaluateBinaryOpUint(expr.Op, left.Value, c.EvaluateConstantUintExpr(decl, right))
		case *tree.ConstFloatExpr:
			return EvaluateBinaryOpFloat(expr.Op, left.Value, c.EvaluateConstantFloatExpr(decl, right))
		case *tree.ConstBoolExpr:
			return EvaluateBinaryOpBool(expr.Op, left.Value, c.EvaluateConstantBoolExpr(decl, right))
		case *tree.ConstStringExpr:
			return EvaluateBinaryOpString(expr.Op, left.Value, c.EvaluateConstantStringExpr(decl, right))
		case *tree.ConstRuneExpr:
			return EvaluateBinaryOpRune(expr.Op, left.Value, c.EvaluateConstantRuneExpr(decl, right))
		default:
			spew.Dump(left, expr.Op, right)
			panic("unreachable")
		}
	case *tree.UnaryExpr:
		operand := c.EvaluateConstantExpr(decl, expr.Expr)

		switch operand := operand.(type) {
		case *tree.ConstIntExpr:
			return &tree.ConstIntExpr{Value: EvaluateUnaryOpInt(expr.Op, operand.Value)}
		case *tree.ConstUintExpr:
			return &tree.ConstUintExpr{Value: c.EvaluateUnaryOpUint(expr.Op, operand.Value)}
		case *tree.ConstFloatExpr:
			return &tree.ConstFloatExpr{Value: EvaluateUnaryOpFloat(expr.Op, operand.Value)}
		case *tree.ConstBoolExpr:
			return &tree.ConstBoolExpr{Value: EvaluateUnaryOpBool(expr.Op, operand.Value)}
		case *tree.ConstStringExpr:
			return &tree.ConstStringExpr{Value: EvaluateUnaryOpString(expr.Op, operand.Value)}
		default:
			panic("unreachable")
		}
	case *tree.CallExpr:
		if len(expr.Args) != 1 {
			panic("conversion expression with multiple arguments")
		}
		switch fun := expr.Func.(type) {
		case *tree.SelectorExpr:
			ty, ok := fun.Expr.(*tree.TypeExpr)
			if !ok {
				break
			}
			imp, ok := ty.Type.(*tree.ImportRef)
			if !ok {
				break
			}
			if imp.ImportPath != "unsafe" {
				break
			}
			switch fun.Sel.Value {
			case "Offsetof":
				return &tree.ConstIntExpr{Value: 1} // TODO omg
			case "Sizeof":
				return &tree.ConstIntExpr{Value: 1} // TODO omg
			default:
				spew.Dump(fun)
				panic("unreachable")
			}
		case *tree.NameExpr:
			switch fun.Name.Value {
			case "len":
				return &tree.ConstIntExpr{Value: 1} // TODO omg
			case "uintptr":
				value := c.EvaluateConstantUintExpr(decl, expr.Args[0])
				return &tree.ConstUintExpr{Value: value}
			case "int":
				value := c.EvaluateConstantIntExpr(decl, expr.Args[0])
				return &tree.ConstIntExpr{Value: value}
			case "int8":
				value := c.EvaluateConstantIntExpr(decl, expr.Args[0])
				if value < math.MinInt8 || value > math.MaxInt8 {
					panic(fmt.Errorf("expected int8, got %v", value))
				}
				return &tree.ConstIntExpr{Value: value}
			case "int64":
				value := c.EvaluateConstantIntExpr(decl, expr.Args[0])
				return &tree.ConstIntExpr{Value: value}
			case "uint":
				value := c.EvaluateConstantUintExpr(decl, expr.Args[0])
				return &tree.ConstUintExpr{Value: value}
			case "uint8":
				value := c.EvaluateConstantUintExpr(decl, expr.Args[0])
				if value > math.MaxUint8 {
					panic(fmt.Errorf("expected uint8, got %v", value))
				}
				return &tree.ConstUintExpr{Value: value}
			case "uint16":
				value := c.EvaluateConstantUintExpr(decl, expr.Args[0])
				if value > math.MaxUint16 {
					panic(fmt.Errorf("expected uint16, got %v", value))
				}
				return &tree.ConstUintExpr{Value: value}
			case "uint32":
				value := c.EvaluateConstantUintExpr(decl, expr.Args[0])
				if value > math.MaxUint32 {
					panic(fmt.Errorf("expected uint32, got %v", value))
				}
				return &tree.ConstUintExpr{Value: value}
			case "uint64":
				value := c.EvaluateConstantUintExpr(decl, expr.Args[0])
				return &tree.ConstUintExpr{Value: value}
			case "float32":
				value := c.EvaluateConstantFloatExpr(decl, expr.Args[0])
				if value < math.SmallestNonzeroFloat32 || value > math.MaxFloat32 {
					panic(fmt.Errorf("expected float32, got %v", value))
				}
				return &tree.ConstFloatExpr{Value: value}
			case "float64":
				value := c.EvaluateConstantFloatExpr(decl, expr.Args[0])
				return &tree.ConstFloatExpr{Value: value}
			default:
				spew.Dump(fun)
				panic("unreachable")
			}
		}
		return c.EvaluateConstantExpr(decl, expr.Args[0])
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func (c *Checker) EvaluateConstantIntExpr(decl *tree.ConstDecl, expr tree.Expr) int64 {
	value := c.EvaluateConstantExpr(decl, expr)
	switch value := value.(type) {
	case *tree.ConstIntExpr:
		return value.Value
	case *tree.ConstUintExpr:
		if value.Value > math.MaxInt64 {
			panic(fmt.Errorf("expected int, got %v", value))
		}
		return int64(value.Value)
	case *tree.ConstFloatExpr:
		if value.Value < math.MinInt64 || value.Value > math.MaxInt64 {
			panic(fmt.Errorf("expected int, got %v", value))
		}
		return int64(value.Value)
	case *tree.ConstRuneExpr:
		return int64(value.Value)
	default:
		panic(fmt.Errorf("expected int, got %v", value))
	}
}

func (c *Checker) EvaluateConstantUintExpr(decl *tree.ConstDecl, expr tree.Expr) uint64 {
	value := c.EvaluateConstantExpr(decl, expr)
	switch value := value.(type) {
	case *tree.ConstUintExpr:
		return value.Value
	case *tree.ConstIntExpr:
		//if value.Value < 0 {
		//	panic(fmt.Errorf("expected uint, got %v", value))
		//}
		return uint64(value.Value)
	case *tree.ConstFloatExpr:
		if value.Value < 0 || value.Value > math.MaxUint64 {
			panic(fmt.Errorf("expected uint, got %v", value))
		}
		return uint64(value.Value)
	default:
		panic(fmt.Errorf("expected uint, got %v", value))
	}
}

func (c *Checker) EvaluateConstantFloatExpr(decl *tree.ConstDecl, expr tree.Expr) float64 {
	value := c.EvaluateConstantExpr(decl, expr)
	switch value := value.(type) {
	case *tree.ConstFloatExpr:
		return value.Value
	case *tree.ConstIntExpr:
		return float64(value.Value)
	case *tree.ConstUintExpr:
		return float64(value.Value)
	default:
		panic(fmt.Errorf("expected float, got %v", value))
	}
}

func (c *Checker) EvaluateConstantBoolExpr(decl *tree.ConstDecl, expr tree.Expr) bool {
	value := c.EvaluateConstantExpr(decl, expr)
	if value, ok := value.(*tree.ConstBoolExpr); ok {
		return value.Value
	}
	panic(fmt.Errorf("expected bool, got %v", value))
}

func (c *Checker) EvaluateConstantStringExpr(decl *tree.ConstDecl, expr tree.Expr) string {
	value := c.EvaluateConstantExpr(decl, expr)
	if value, ok := value.(*tree.ConstStringExpr); ok {
		return value.Value
	}
	panic(fmt.Errorf("expected string, got %v", value))
}

func (c *Checker) EvaluateConstantRuneExpr(decl *tree.ConstDecl, expr tree.Expr) rune {
	value := c.EvaluateConstantExpr(decl, expr)
	switch value := value.(type) {
	case *tree.ConstRuneExpr:
		return value.Value
	case *tree.ConstIntExpr:
		if value.Value < 0 || value.Value > math.MaxInt32 {
			panic(fmt.Errorf("expected rune, got %v", value))
		}
		return rune(value.Value)
	case *tree.ConstUintExpr:
		if value.Value > math.MaxInt32 {
			panic(fmt.Errorf("expected rune, got %v", value))
		}
		return rune(value.Value)
	default:
		panic(fmt.Errorf("expected rune, got %v", value))
	}
}

func EvaluateBinaryOpInt(op tree.BinaryOp, left int64, right int64) tree.Expr {
	if v, err := EvaluateBinaryOpNumericCompare(op, left, right); err == nil {
		return &tree.ConstBoolExpr{Value: v}
	}
	if v, err := EvaluateBinaryOpIntegerArithmetic(op, left, right); err == nil {
		return &tree.ConstIntExpr{Value: v}
	}
	panic(fmt.Sprintf("unsupported int binary op %v", op))
}

func EvaluateBinaryOpUint(op tree.BinaryOp, left, right uint64) tree.Expr {
	if v, err := EvaluateBinaryOpNumericCompare(op, left, right); err == nil {
		return &tree.ConstBoolExpr{Value: v}
	}
	if v, err := EvaluateBinaryOpIntegerArithmetic(op, left, right); err == nil {
		return &tree.ConstUintExpr{Value: v}
	}
	panic(fmt.Sprintf("unsupported uint binary op %v", op))
}

func EvaluateBinaryOpFloat(op tree.BinaryOp, left, right float64) tree.Expr {
	if v, err := EvaluateBinaryOpNumericCompare(op, left, right); err == nil {
		return &tree.ConstBoolExpr{Value: v}
	}
	if v, err := EvaluateBinaryOpFloatArithmetic(op, left, right); err == nil {
		return &tree.ConstFloatExpr{Value: v}
	}
	panic(fmt.Sprintf("unsupported float binary op %v", op))
}

func EvaluateBinaryOpRune(op tree.BinaryOp, left rune, right rune) tree.Expr {
	if v, err := EvaluateBinaryOpNumericCompare(op, left, right); err == nil {
		return &tree.ConstBoolExpr{Value: v}
	}
	if v, err := EvaluateBinaryOpIntegerArithmetic(op, left, right); err == nil {
		return &tree.ConstRuneExpr{Value: v}
	}
	panic(fmt.Sprintf("unsupported int binary op %v", op))
}

func EvaluateBinaryOpNumericCompare[T int64 | uint64 | float64 | rune](op tree.BinaryOp, left, right T) (bool, error) {
	switch op {
	case tree.BinaryOpEq:
		return left == right, nil
	case tree.BinaryOpNeq:
		return left != right, nil
	case tree.BinaryOpLt:
		return left < right, nil
	case tree.BinaryOpLte:
		return left <= right, nil
	case tree.BinaryOpGt:
		return left > right, nil
	case tree.BinaryOpGte:
		return left >= right, nil
	default:
		return false, fmt.Errorf("unsupported binary op %v", op)
	}
}

func EvaluateBinaryOpIntegerArithmetic[T int64 | uint64 | rune](op tree.BinaryOp, left, right T) (T, error) {
	switch op {
	case tree.BinaryOpAdd:
		return left + right, nil
	case tree.BinaryOpSub:
		return left - right, nil
	case tree.BinaryOpMul:
		return left * right, nil
	case tree.BinaryOpQuo:
		return left / right, nil
	case tree.BinaryOpRem:
		return left % right, nil
	case tree.BinaryOpAnd:
		return left & right, nil
	case tree.BinaryOpOr:
		return left | right, nil
	case tree.BinaryOpXor:
		return left ^ right, nil
	case tree.BinaryOpAndNot:
		return left &^ right, nil
	case tree.BinaryOpShl:
		return left << uint(right), nil
	case tree.BinaryOpShr:
		return left >> uint(right), nil
	default:
		return 0, fmt.Errorf("unsupported binary op %v", op)
	}
}

func EvaluateBinaryOpFloatArithmetic[T float64](op tree.BinaryOp, left, right T) (T, error) {
	switch op {
	case tree.BinaryOpAdd:
		return left + right, nil
	case tree.BinaryOpSub:
		return left - right, nil
	case tree.BinaryOpMul:
		return left * right, nil
	case tree.BinaryOpQuo:
		return left / right, nil
	default:
		return 0, fmt.Errorf("unsupported binary op %v", op)
	}
}

func EvaluateBinaryOpBool(op tree.BinaryOp, left, right bool) tree.Expr {
	switch op {
	case tree.BinaryOpLAnd:
		return &tree.ConstBoolExpr{Value: left && right}
	case tree.BinaryOpLOr:
		return &tree.ConstBoolExpr{Value: left || right}
	default:
		panic(fmt.Sprintf("unsupported bool binary op %v", op))
	}
}

func EvaluateBinaryOpString(op tree.BinaryOp, left, right string) tree.Expr {
	switch op {
	case tree.BinaryOpAdd:
		return &tree.ConstStringExpr{Value: left + right}
	default:
		panic(fmt.Sprintf("unsupported string binary op %v", op))
	}
}

func EvaluateUnaryOpInt(op tree.UnaryOp, value int64) int64 {
	return EvaluateUnaryOpInteger(op, value)
}

func (c *Checker) EvaluateUnaryOpUint(op tree.UnaryOp, value uint64) uint64 {
	return EvaluateUnaryOpInteger(op, value)
}

func EvaluateUnaryOpFloat(op tree.UnaryOp, value float64) float64 {
	switch op {
	case tree.UnaryOpPos:
		return value
	case tree.UnaryOpNeg:
		return -value
	default:
		panic(fmt.Sprintf("unsupported int unary op %v", op))
	}
}

func EvaluateUnaryOpInteger[T int64 | uint64](op tree.UnaryOp, value T) T {
	switch op {
	case tree.UnaryOpPos:
		return value
	case tree.UnaryOpNeg:
		return -value
	case tree.UnaryOpBitNot:
		return ^value
	default:
		panic(fmt.Sprintf("unsupported int unary op %v", op))
	}
}

func EvaluateUnaryOpBool(op tree.UnaryOp, value bool) bool {
	switch op {
	case tree.UnaryOpNot:
		return !value
	default:
		panic(fmt.Sprintf("unsupported bool unary op %v", op))
	}
}

func EvaluateUnaryOpString(op tree.UnaryOp, value string) string {
	panic(fmt.Sprintf("unsupported string unary op %v", op))
}
