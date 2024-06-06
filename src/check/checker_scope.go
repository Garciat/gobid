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
	copy := c.Copy()
	copy.TyCtx = c.TyCtx.Fork(kind)
	copy.VarCtx = c.VarCtx.Fork(kind)
	return copy
}

func (c *Checker) BeginPackageScope(pkg *source.Package) *Checker {
	copy := c._BeginScope(ScopeKindPackage)
	copy.CurPkg = pkg
	return copy
}

func (c *Checker) BeginFileScope(file *source.FileDef) *Checker {
	copy := c._BeginScope(ScopeKindFile)
	copy.CurFile = file
	return copy
}

func (c *Checker) BeginTypeScope(decl *tree.TypeDecl) *Checker {
	copy := c._BeginScope(ScopeKindType)
	copy.CurTy = decl
	return copy
}

func (c *Checker) BeginFunctionScope(fn *tree.Signature) *Checker {
	copy := c._BeginScope(ScopeKindFunction)
	copy.CurFn = fn
	return copy
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
