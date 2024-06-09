package check

import (
	"fmt"
	. "github.com/garciat/gobid/common"
	"github.com/garciat/gobid/tree"
	"strings"
)

func (c *Checker) FreshTypeName(prefix string) Identifier {
	*c.Fresh = *c.Fresh + 1
	return NewIdentifier(fmt.Sprintf("%s%d", prefix, *c.Fresh))
}

const instancePrefix = "@T"

func (c *Checker) NewFreeType() *tree.FreeTypeVar {
	name := c.FreshTypeName(instancePrefix)
	ty := &tree.FreeTypeVar{Name: name}
	c.DefineType(name, ty)
	return ty
}

func (c *Checker) GetCurrentScopeTypeInstantiations() []*tree.TypeParam {
	var result []*tree.TypeParam
	for _, ty := range c.Ctx.Types {
		if typeParam, ok := ty.(*tree.TypeParam); ok {
			if strings.HasPrefix(typeParam.Name.Value, instancePrefix) {
				result = append(result, typeParam)
			}
		}
	}
	return result
}

func (c *Checker) Lookup(name Identifier) tree.Type {
	ty, ok := c.Ctx.Lookup(name)
	if ok {
		return ty
	}
	ty, ok = c.Builtins.Lookup(name)
	if ok {
		return ty
	}
	panic(fmt.Errorf("undefined: %v", name))
}

func (c *Checker) LookupConst(name Identifier) tree.Expr {
	expr, ok := c.Ctx.LookupConst(name)
	if ok {
		return expr
	}
	panic(fmt.Errorf("undefined constant: %v", name))
}

func (c *Checker) PackageLookup(ip ImportPath, name Identifier) tree.Type {
	pkg, ok := c.PackageSymbols[ip]
	if !ok {
		panic(fmt.Errorf("package not loaded: %v", ip))
	}
	ty, ok := pkg.Lookup(name)
	if !ok {
		panic(fmt.Errorf(`undefined: "%v".%v`, ip, name))
	}
	return ty
}

func (c *Checker) PackageLookupConst(ip ImportPath, name Identifier) tree.Expr {
	pkg, ok := c.PackageSymbols[ip]
	if !ok {
		panic(fmt.Errorf("package not loaded: %v", ip))
	}
	expr, ok := pkg.LookupConst(name)
	if !ok {
		panic(fmt.Errorf(`undefined: "%v".%v`, ip, name))
	}
	return expr
}

func (c *Checker) DefineImport(decl *tree.ImportDecl) {
	//fmt.Printf("DEFINING import %v\n", decl.ImportPath)
	c.Ctx.Def(NewIdentifier(decl.EffectiveName().Value), &tree.ImportType{ImportPath: decl.ImportPath})
}

func (c *Checker) DefineValue(name Identifier, ty tree.Type) {
	CheckerPrintf("DEFINING %v = %v\n", name, ty)
	if c.Ctx.ScopeKind == ScopeKindFile {
		Assert(c.Ctx.Parent.ScopeKind == ScopeKindPackage, "expected package scope")
		c.Ctx.Parent.Def(name, ty)
	} else {
		c.Ctx.Def(name, ty)
	}
}

func (c *Checker) DefineType(name Identifier, ty tree.Type) {
	CheckerPrintf("DEFINING TYPE %v = %v\n", name, ty)

	var target *TypeContext
	if c.Ctx.ScopeKind == ScopeKindFile {
		Assert(c.Ctx.Parent.ScopeKind == ScopeKindPackage, "expected package scope")
		target = c.Ctx.Parent
	} else {
		target = c.Ctx
	}

	target.DefType(name, ty)
}

func (c *Checker) DefineFunction(name Identifier, ty *tree.FunctionType) {
	if name == NewIdentifier("init") {
		// init functions are not defined
		// TODO check signature
		CheckerPrintf("IGNORING DEFINITION init %v\n", ty.Signature)
		return
	}
	CheckerPrintf("DEFINING func %v%v\n", name, ty.Signature)
	Assert(c.Ctx.Parent.ScopeKind == ScopeKindPackage, "expected package scope")
	c.Ctx.Parent.Def(name, ty)
}

func (c *Checker) DefineMethod(holder, name Identifier, ty *tree.MethodType) {
	var prefix string
	if ty.PointerReceiver {
		prefix = "*"
	}
	CheckerPrintf("DEFINING METHOD func (%s%v) %v%v\n", prefix, holder, name, ty.Type.Signature)
	methodName := NewIdentifier(fmt.Sprintf("%s.%s", holder.Value, name.Value))
	Assert(c.Ctx.Parent.ScopeKind == ScopeKindPackage, "expected package scope")
	c.Ctx.Parent.Def(methodName, ty)
}

func (c *Checker) BuiltinValue(name string) tree.Type {
	ty, ok := c.Builtins.Lookup(NewIdentifier(name))
	if !ok {
		panic(fmt.Errorf("undefined builtin: %v", name))
	}
	return ty
}

func (c *Checker) BuiltinType(name string) tree.Type {
	ty, ok := c.Builtins.Lookup(NewIdentifier(name))
	if !ok {
		panic(fmt.Errorf("undefined builtin: %v", name))
	}
	realTy, ok := ty.(*tree.TypeOfType)
	if !ok {
		panic(fmt.Errorf("not a builtin type: %v", ty))
	}
	return realTy.Type
}
