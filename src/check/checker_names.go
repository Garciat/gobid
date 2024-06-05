package check

import (
	"fmt"
	. "github.com/garciat/gobid/common"
	"github.com/garciat/gobid/tree"
)

func (c *Checker) FreshTypeName() Identifier {
	*c.Fresh = *c.Fresh + 1
	return Identifier{Value: fmt.Sprintf("@T%d", *c.Fresh)}
}

func (c *Checker) Lookup(name Identifier) tree.Type {
	ty, ok := c.VarCtx.Lookup(name)
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
	expr, ok := c.VarCtx.LookupConst(name)
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
	c.VarCtx.Def(NewIdentifier(decl.EffectiveName().Value), &tree.ImportType{ImportPath: decl.ImportPath})
}

func (c *Checker) DefineValue(name Identifier, ty tree.Type) {
	fmt.Printf("DEFINING %v = %v\n", name, ty)
	if c.VarCtx.ScopeKind == ScopeKindFile {
		Assert(c.VarCtx.Parent.ScopeKind == ScopeKindPackage, "expected package scope")
		c.VarCtx.Parent.Def(name, ty)
	} else {
		c.VarCtx.Def(name, ty)
	}
}

func (c *Checker) DefineConstant(name Identifier, ty tree.Type, decl *tree.ConstDecl) {
	fmt.Printf("DEFINING %v = %v\n", name, ty)

	var target *VarContext
	if c.VarCtx.ScopeKind == ScopeKindFile {
		Assert(c.VarCtx.Parent.ScopeKind == ScopeKindPackage, "expected package scope")
		target = c.VarCtx.Parent

		// Only evaluate constants in package scope
		value := c.EvaluateConstant(c.ResolveType(ty), decl)
		fmt.Printf("EVALUATED %v = %v\n", name, value)
		target.DefConst(name, ty, value)
	} else {
		target = c.VarCtx
		target.Def(name, ty) // TODO mark as constant? (cannot reassign)
	}
}

func (c *Checker) DefineType(name Identifier, ty tree.Type) {
	fmt.Printf("DEFINING TYPE %v = %v\n", name, ty)

	var target *VarContext
	if c.VarCtx.ScopeKind == ScopeKindFile {
		Assert(c.VarCtx.Parent.ScopeKind == ScopeKindPackage, "expected package scope")
		target = c.VarCtx.Parent
	} else {
		target = c.VarCtx
	}

	target.DefType(name, ty)
}

func (c *Checker) DefineFunction(name Identifier, ty *tree.FunctionType) {
	if name == NewIdentifier("init") {
		// init functions are not defined
		// TODO check signature
		fmt.Printf("IGNORING DEFINITION init %v\n", ty.Signature)
		return
	}
	fmt.Printf("DEFINING func %v%v\n", name, ty.Signature)
	Assert(c.VarCtx.Parent.ScopeKind == ScopeKindPackage, "expected package scope")
	c.VarCtx.Parent.Def(name, ty)
}

func (c *Checker) DefineMethod(holder, name Identifier, ty *tree.MethodType) {
	var prefix string
	if ty.PointerReceiver {
		prefix = "*"
	}
	fmt.Printf("DEFINING METHOD func (%s%v) %v%v\n", prefix, holder, name, ty.Type.Signature)
	methodName := NewIdentifier(fmt.Sprintf("%s.%s", holder.Value, name.Value))
	Assert(c.VarCtx.Parent.ScopeKind == ScopeKindPackage, "expected package scope")
	c.VarCtx.Parent.Def(methodName, ty)
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
