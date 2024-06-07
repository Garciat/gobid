package check

import (
	. "github.com/garciat/gobid/common"
	"github.com/garciat/gobid/parse"
	"github.com/garciat/gobid/tree"
)

func MakeUnsafePackage() *VarContext {
	scope := NewVarContext()
	scope.DefType(NewIdentifier("Pointer"), tree.BuiltinTypeUnsafePointer)
	scope.Def(NewIdentifier("Sizeof"), parse.TypeOf("func(interface{}) uintptr"))
	scope.Def(NewIdentifier("Offsetof"), parse.TypeOf("func(interface{}) uintptr"))
	scope.Def(NewIdentifier("Alignof"), parse.TypeOf("func(interface{}) uintptr"))
	scope.Def(NewIdentifier("Add"), parse.TypeOf("func(Pointer, int) Pointer"))
	scope.Def(NewIdentifier("Slice"), parse.FuncTypeOf("func(*T, int) []T").WithTypeParams("T"))
	scope.Def(NewIdentifier("SliceData"), parse.FuncTypeOf("func([]T) *T").WithTypeParams("T"))
	scope.Def(NewIdentifier("String"), parse.TypeOf("func(*byte, int) string"))
	scope.Def(NewIdentifier("StringData"), parse.TypeOf("func(string) *byte"))
	return scope
}
