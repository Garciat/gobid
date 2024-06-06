package check

import (
	. "github.com/garciat/gobid/common"
	"github.com/garciat/gobid/parse"
	"github.com/garciat/gobid/tree"
)

func MakeBuiltins() *VarContext {
	scope := NewVarContext()

	scope.DefBuiltinType("bool")
	scope.Def(NewIdentifier("true"), &tree.UntypedConstantType{Kind: tree.UntypedConstantBool})
	scope.Def(NewIdentifier("false"), &tree.UntypedConstantType{Kind: tree.UntypedConstantBool})

	uint8Ty := scope.DefBuiltinNumericType("uint8")
	scope.DefBuiltinNumericType("uint16")
	scope.DefBuiltinNumericType("uint32")
	scope.DefBuiltinNumericType("uint64")

	scope.DefBuiltinNumericType("int8")
	scope.DefBuiltinNumericType("int16")
	int32Ty := scope.DefBuiltinNumericType("int32")
	scope.DefBuiltinNumericType("int64")

	scope.DefBuiltinNumericType("float32")
	scope.DefBuiltinNumericType("float64")

	scope.DefBuiltinNumericType("complex64")
	scope.DefBuiltinNumericType("complex128")

	scope.DefBuiltinType("string")

	scope.DefBuiltinNumericType("int")
	scope.DefBuiltinNumericType("uint")
	scope.DefBuiltinNumericType("uintptr")

	scope.Def(NewIdentifier("byte"), uint8Ty) // already tree.TypeOfType
	scope.Def(NewIdentifier("rune"), int32Ty) // already tree.TypeOfType

	scope.DefType(NewIdentifier("any"), tree.EmptyInterface())

	// TODO comparable constraint

	scope.Def(NewIdentifier("iota"), tree.UntypedInt())

	scope.Def(NewIdentifier("nil"), &tree.NilType{})

	scope.DefBuiltinFunction("append")
	scope.DefBuiltinFunction("copy")
	scope.DefBuiltinFunction("delete")
	scope.DefBuiltinFunction("len")
	scope.DefBuiltinFunction("cap")
	scope.DefBuiltinFunction("make")
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

	// TODO Ordered constraint for these
	scope.Def(NewIdentifier("max"), parse.ParseFuncType("func(...T) T").WithTypeParams("T"))
	scope.Def(NewIdentifier("min"), parse.ParseFuncType("func(...T) T").WithTypeParams("T"))

	scope.DefNamedType(NewIdentifier("error"), parse.ParseType("interface{Error() string}"))

	return scope
}
