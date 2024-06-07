package check

import (
	. "github.com/garciat/gobid/common"
	"github.com/garciat/gobid/parse"
	"github.com/garciat/gobid/tree"
)

func MakeBuiltins() *VarContext {
	scope := NewVarContext()

	scope.DefBuiltinType(tree.BuiltinTypeBool)
	scope.Def(NewIdentifier("true"), tree.UntypedConstantBoolType)
	scope.Def(NewIdentifier("false"), tree.UntypedConstantBoolType)

	scope.DefBuiltinType(tree.BuiltinTypeUint8)
	scope.DefBuiltinType(tree.BuiltinTypeUint16)
	scope.DefBuiltinType(tree.BuiltinTypeUint32)
	scope.DefBuiltinType(tree.BuiltinTypeUint64)

	scope.DefBuiltinType(tree.BuiltinTypeInt8)
	scope.DefBuiltinType(tree.BuiltinTypeInt16)
	scope.DefBuiltinType(tree.BuiltinTypeInt32)
	scope.DefBuiltinType(tree.BuiltinTypeInt64)

	scope.DefBuiltinType(tree.BuiltinTypeFloat32)
	scope.DefBuiltinType(tree.BuiltinTypeFloat64)

	scope.DefBuiltinType(tree.BuiltinTypeComplex64)
	scope.DefBuiltinType(tree.BuiltinTypeComplex128)

	scope.DefBuiltinType(tree.BuiltinTypeString)

	scope.DefBuiltinType(tree.BuiltinTypeInt)
	scope.DefBuiltinType(tree.BuiltinTypeUint)
	scope.DefBuiltinType(tree.BuiltinTypeUintptr)

	scope.DefType(NewIdentifier("byte"), tree.BuiltinTypeUint8)
	scope.DefType(NewIdentifier("rune"), tree.BuiltinTypeInt32)

	scope.DefType(NewIdentifier("any"), tree.EmptyInterface())

	scope.DefBuiltinType(tree.BuiltinTypeComparable)

	scope.Def(NewIdentifier("iota"), tree.UntypedConstantIntType)

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
	scope.Def(NewIdentifier("max"), parse.FuncTypeOf("func(...T) T").WithTypeParams("T"))
	scope.Def(NewIdentifier("min"), parse.FuncTypeOf("func(...T) T").WithTypeParams("T"))

	scope.DefNamedType("builtin", NewIdentifier("error"), parse.TypeOf("interface{Error() string}"))

	return scope
}
