package check

import (
	"github.com/garciat/gobid/source"
	"github.com/garciat/gobid/tree"
)

type ScopeKind int

const (
	ScopeKindPackage ScopeKind = iota
	ScopeKindFile
	ScopeKindType
	ScopeKindFunction
	ScopeKindBlock
)

func (k ScopeKind) String() string {
	switch k {
	case ScopeKindPackage:
		return "ScopeKindPackage"
	case ScopeKindFile:
		return "ScopeKindFile"
	case ScopeKindType:
		return "ScopeKindType"
	case ScopeKindFunction:
		return "ScopeKindFunction"
	case ScopeKindBlock:
		return "ScopeKindBlock"
	default:
		panic("unreachable")
	}
}

func (c *Checker) _BeginScope(kind ScopeKind) *Checker {
	scope := c.Copy()
	scope.TyCtx = c.TyCtx.Fork(kind)
	scope.VarCtx = c.VarCtx.Fork(kind)
	return scope
}

func (c *Checker) BeginPackageScope(pkg *source.Package) *Checker {
	scope := c._BeginScope(ScopeKindPackage)
	scope.CurPkg = pkg
	return scope
}

func (c *Checker) BeginFileScope(file *source.FileDef) *Checker {
	scope := c._BeginScope(ScopeKindFile)
	scope.CurFile = file
	return scope
}

func (c *Checker) BeginTypeScope(decl *tree.TypeDecl) *Checker {
	scope := c._BeginScope(ScopeKindType)
	scope.CurTy = decl
	return scope
}

func (c *Checker) BeginFunctionScope(fn *tree.Signature) *Checker {
	scope := c._BeginScope(ScopeKindFunction)
	scope.CurFn = fn
	return scope
}

func (c *Checker) BeginBlockScope() *Checker {
	return c._BeginScope(ScopeKindBlock)
}

func (c *Checker) AssertInFunctionScope() *tree.Signature {
	if c.CurFn == nil {
		panic("not in function scope")
	}
	return c.CurFn
}
