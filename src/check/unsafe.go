package check

import (
	. "github.com/garciat/gobid/common"
	"github.com/garciat/gobid/parse"
	"github.com/garciat/gobid/tree"
)

func MakeUnsafePackage() *VarContext {
	scope := EmptyVarContext()
	scope.DefType(NewIdentifier("Pointer"), tree.NewBuiltinType("Pointer"))
	scope.Def(NewIdentifier("Sizeof"), parse.ParseType("func(interface{}) uintptr"))
	scope.Def(NewIdentifier("Offsetof"), parse.ParseType("func(interface{}) uintptr"))
	scope.Def(NewIdentifier("Alignof"), parse.ParseType("func(interface{}) uintptr"))
	scope.Def(NewIdentifier("Add"), parse.ParseType("func(Pointer, int) Pointer"))
	scope.Def(NewIdentifier("Slice"), parse.ParseFuncType("func(T, int) []T").WithTypeParams("T"))
	scope.Def(NewIdentifier("SliceData"), parse.ParseFuncType("func([]T) *T").WithTypeParams("T"))
	scope.Def(NewIdentifier("String"), parse.ParseType("func(*byte, int) string"))
	scope.Def(NewIdentifier("StringData"), parse.ParseType("func(string) *byte"))
	return scope
}
