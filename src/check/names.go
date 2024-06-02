package check

import (
	"fmt"
	"github.com/davecgh/go-spew/spew"
	"github.com/garciat/gobid/algos"
	"github.com/garciat/gobid/common"
	"github.com/garciat/gobid/source"
	"github.com/garciat/gobid/tree"
)

type Ref struct {
	Expr *tree.Expr
	Type *tree.Type
}

type Deps = common.Set[Ref]

var NoDeps = Deps{}

func NewVarRef(ref *tree.Expr) Deps {
	result := Deps{}
	result.Add(Ref{Expr: ref})
	return result
}

func NewTypeRef(ref *tree.Type) Deps {
	result := Deps{}
	result.Add(Ref{Type: ref})
	return result
}

type NameInfo struct {
	Scope        ScopeKind
	Kind         common.DeclKind
	Name         common.Identifier
	Dependencies Deps
}

type NameContext struct {
	Parent    *NameContext
	Children  []*NameContext
	ScopeKind ScopeKind
	Scope     any
	Names     map[common.Identifier]*NameInfo
}

func (ctx *NameContext) Define(kind common.DeclKind, name common.Identifier, deps Deps) *NameInfo {
	var target *NameContext

	switch kind {
	case common.DeclKindImport:
		common.Assert(ctx.ScopeKind == ScopeKindFile, "import decl in non-file scope")
		common.Assert(ctx.Parent.ScopeKind == ScopeKindPackage, "BUG")
		target = ctx.Parent
	case common.DeclKindConst, common.DeclKindVar, common.DeclKindFunc, common.DeclKindAlias, common.DeclKindType:
		if ctx.ScopeKind == ScopeKindFile {
			common.Assert(ctx.Parent.ScopeKind == ScopeKindPackage, "BUG")
			target = ctx.Parent
		} else {
			target = ctx
		}
	case common.DeclKindMethod:
		common.Assert(ctx.ScopeKind == ScopeKindFile, "method decl in non-file scope")
		target = ctx.Parent
	case common.DeclKindTypeParam, common.DeclKindParam:
		// always locals
		target = ctx
	default:
		spew.Dump(kind)
		panic("unreachable")
	}

	info := &NameInfo{
		Scope:        target.ScopeKind,
		Kind:         kind,
		Name:         name,
		Dependencies: deps,
	}

	target.Names[name] = info

	return info
}

// ====================

func MakeBuiltinContext() *NameContext {
	ctx := &NameContext{
		Parent:    nil,
		Children:  nil,
		ScopeKind: ScopeKindBuiltin,
		Scope:     nil,
		Names:     make(map[common.Identifier]*NameInfo),
	}

	builtins := MakeBuiltins()

	for name, ty := range builtins.Types {
		var kind common.DeclKind
		switch ty := ty.(type) {
		case *tree.TypeOfType:
			switch ty.Type.(type) {
			case *tree.TypeBuiltin:
				kind = common.DeclKindType
			default:
				kind = common.DeclKindAlias
			}
		case *tree.BuiltinFunctionType:
			kind = common.DeclKindFunc
		default:
			kind = common.DeclKindConst
		}
		ctx.Names[name] = &NameInfo{
			Scope:        ScopeKindBuiltin,
			Kind:         kind,
			Name:         name,
			Dependencies: NoDeps,
		}
	}

	return ctx
}

// ====================

type NameResolver struct {
	PackagePath common.ImportPath
	Imports     map[common.Identifier]common.ImportPath
	Context     *NameContext
}

func ResolveNames(pkg *source.Package) {
	fmt.Printf("=== ResolveNames(%v) ===\n", pkg.ImportPath)
	nr := &NameResolver{
		PackagePath: pkg.ImportPath,
		Imports:     make(map[common.Identifier]common.ImportPath),
		Context: &NameContext{
			Parent:    MakeBuiltinContext(),
			Children:  nil,
			ScopeKind: ScopeKindPackage,
			Scope:     pkg,
			Names:     make(map[common.Identifier]*NameInfo),
		},
	}
	nr.BuildGraph(pkg)

	nr.CheckCycles()

	reps := make(map[common.Identifier]tree.Node)
	for _, name := range nr.Context.Parent.Names {
		reps[name.Name] = nr.MakeReplacement(name)
	}

	nr.BindContext(nr.Context, reps)
}

func (nr *NameResolver) Fork(kind ScopeKind, scope any) *NameResolver {
	parent := nr.Context
	child := &NameContext{
		Parent:    parent,
		Children:  nil,
		ScopeKind: kind,
		Scope:     scope,
		Names:     make(map[common.Identifier]*NameInfo),
	}
	parent.Children = append(parent.Children, child)
	return &NameResolver{
		PackagePath: nr.PackagePath,
		Imports:     nr.Imports,
		Context:     child,
	}
}

// ====================

func (nr *NameResolver) BuildGraph(pkg *source.Package) {
	for _, file := range pkg.Files {
		scopeFile := nr.Fork(ScopeKindFile, file)
		for _, decl := range file.Decls {
			scopeFile.GraphWalkDecl(decl)
		}
	}
}

func (nr *NameResolver) GraphWalkDecl(decl tree.Decl) Deps {
	switch decl := decl.(type) {
	case *tree.ImportDecl:
		return nr.GraphWalkImportDecl(decl)
	case *tree.ConstDecl:
		return nr.GraphWalkConstDecl(decl)
	case *tree.VarDecl:
		return nr.GraphWalkVarDecl(decl)
	case *tree.TypeDecl:
		return nr.GraphWalkTypeDecl(decl)
	case *tree.AliasDecl:
		return nr.GraphWalkAliasDecl(decl)
	case *tree.FunctionDecl:
		return nr.GraphWalkFunctionDecl(decl)
	case *tree.MethodDecl:
		return nr.GraphWalkMethodDecl(decl)
	default:
		panic(fmt.Sprintf("unknown decl type: %T", decl))
	}
}

func (nr *NameResolver) GraphWalkImportDecl(decl *tree.ImportDecl) Deps {
	nr.Context.Define(common.DeclKindImport, decl.EffectiveName(), NoDeps)
	nr.Imports[decl.EffectiveName()] = decl.ImportPath
	return NoDeps
}

func (nr *NameResolver) GraphWalkConstDecl(decl *tree.ConstDecl) Deps {
	deps := common.MergeSets(
		nr.GraphWalkType(&decl.Type),
		nr.GraphWalkExpr(&decl.Value),
	)

	nr.Context.Define(common.DeclKindConst, decl.Name, deps)

	return deps
}

func (nr *NameResolver) GraphWalkVarDecl(decl *tree.VarDecl) Deps {
	deps := common.MergeSets(
		nr.GraphWalkType(&decl.Type),
		nr.GraphWalkExpr(&decl.Expr),
	)

	for _, name := range decl.Names {
		nr.Context.Define(common.DeclKindVar, name, deps)
	}

	return deps
}

func (nr *NameResolver) GraphWalkTypeDecl(decl *tree.TypeDecl) Deps {
	scope := nr.Fork(ScopeKindType, decl)

	deps := common.MergeSets(
		scope.GraphWalkTypeParamList(decl.TypeParams),
		scope.GraphWalkType(&decl.Type),
	)

	nr.Context.Define(common.DeclKindType, decl.Name, deps)

	return deps
}

func (nr *NameResolver) GraphWalkAliasDecl(decl *tree.AliasDecl) Deps {
	deps := nr.GraphWalkType(&decl.Type)
	nr.Context.Define(common.DeclKindAlias, decl.Name, deps)
	return deps
}

func (nr *NameResolver) GraphWalkFunctionDecl(decl *tree.FunctionDecl) Deps {
	scope := nr.Fork(ScopeKindFunction, decl)

	deps := scope.GraphWalkSignature(decl.Signature)

	nr.Context.Define(common.DeclKindFunc, decl.Name, deps)

	return deps
}

func (nr *NameResolver) GraphWalkMethodDecl(decl *tree.MethodDecl) Deps {
	return NoDeps
}

func (nr *NameResolver) GraphWalkSignature(sig *tree.Signature) Deps {
	return common.MergeSets(
		nr.GraphWalkTypeParamList(sig.TypeParams),
		nr.GraphWalkParamList(sig.Params),
		nr.GraphWalkParamList(sig.Results),
	)
}

func (nr *NameResolver) GraphWalkTypeParamList(list *tree.TypeParamList) Deps {
	allDeps := make([]Deps, len(list.Params))
	for i, param := range list.Params {
		deps := nr.GraphWalkInterfaceType(param.Constraint)
		nr.Context.Define(common.DeclKindTypeParam, param.Name, NoDeps)
		allDeps[i] = deps
	}
	return common.MergeSets(allDeps...)
}

func (nr *NameResolver) GraphWalkParamList(list *tree.ParameterList) Deps {
	allDeps := make([]Deps, len(list.Params))
	for i, param := range list.Params {
		deps := nr.GraphWalkType(&param.Type)
		nr.Context.Define(common.DeclKindParam, param.Name, NoDeps)
		allDeps[i] = deps
	}
	return common.MergeSets(allDeps...)
}

func (nr *NameResolver) GraphWalkType(arg *tree.Type) Deps {
	if *arg == nil {
		return NoDeps
	}
	switch ty := (*arg).(type) {
	case *tree.NamedType:
		panic("doesn't exist yet")
	case *tree.QualIdentifier:
		return NoDeps
	case *tree.TypeName:
		return NewTypeRef(arg)
	case *tree.TypeApplication:
		return nr.GraphWalkTypeApplication(ty)
	case *tree.InterfaceType:
		return nr.GraphWalkInterfaceType(ty)
	case *tree.StructType:
		return nr.GraphWalkStructType(ty)
	case *tree.ArrayType:
		return nr.GraphWalkArrayType(ty)
	case *tree.FunctionType:
		return nr.GraphWalkSignature(ty.Signature)
	case *tree.SliceType:
		return nr.GraphWalkType(&ty.ElemType)
	case *tree.MapType:
		return nr.GraphWalkMapType(ty)
	case *tree.PointerType:
		return nr.GraphWalkType(&ty.BaseType)
	case *tree.ChannelType:
		return nr.GraphWalkType(&ty.ElemType)
	default:
		spew.Dump(ty)
		panic("unreachable")
	}
}

func (nr *NameResolver) GraphWalkTypeApplication(ty *tree.TypeApplication) Deps {
	allDeps := make([]Deps, len(ty.Args)+1)
	for i, arg := range ty.Args {
		deps := nr.GraphWalkType(&arg)
		allDeps[i] = deps
	}
	allDeps[len(ty.Args)] = nr.GraphWalkType(&ty.Type)
	return common.MergeSets(allDeps...)
}

func (nr *NameResolver) GraphWalkInterfaceType(ty *tree.InterfaceType) Deps {
	allDeps := make([]Deps, len(ty.Methods))
	for i, method := range ty.Methods {
		deps := nr.GraphWalkSignature(method.Type.Signature)
		allDeps[i] = deps
	}
	for _, constraint := range ty.Constraints {
		for _, param := range constraint.TypeElem.Union {
			deps := nr.GraphWalkType(&param.Type)
			allDeps = append(allDeps, deps)
		}
	}
	return common.MergeSets(allDeps...)
}

func (nr *NameResolver) GraphWalkStructType(ty *tree.StructType) Deps {
	allDeps := make([]Deps, len(ty.Fields))
	for i, field := range ty.Fields {
		deps := nr.GraphWalkType(&field.Type)
		allDeps[i] = deps
	}
	return common.MergeSets(allDeps...)
}

func (nr *NameResolver) GraphWalkArrayType(ty *tree.ArrayType) Deps {
	return common.MergeSets(
		nr.GraphWalkExpr(&ty.Len),
		nr.GraphWalkType(&ty.ElemType),
	)
}

func (nr *NameResolver) GraphWalkMapType(ty *tree.MapType) Deps {
	return common.MergeSets(
		nr.GraphWalkType(&ty.KeyType),
		nr.GraphWalkType(&ty.ElemType),
	)
}

func (nr *NameResolver) GraphWalkExpr(arg *tree.Expr) Deps {
	if *arg == nil {
		return NoDeps
	}
	switch expr := (*arg).(type) {
	case *tree.TypeExpr:
		return nr.GraphWalkType(&expr.Type)
	case *tree.BinaryExpr:
		return common.MergeSets(
			nr.GraphWalkExpr(&expr.Left),
			nr.GraphWalkExpr(&expr.Right),
		)
	case *tree.UnaryExpr:
		return nr.GraphWalkExpr(&expr.Expr)
	case *tree.StarExpr:
		return nr.GraphWalkExpr(&expr.Expr)
	case *tree.AddressExpr:
		return nr.GraphWalkExpr(&expr.Expr)
	case *tree.ConversionExpr:
		return nr.GraphWalkExpr(&expr.Expr)
	case *tree.SelectorExpr:
		return nr.GraphWalkExpr(&expr.Expr)
	case *tree.IndexExpr:
		return common.MergeSets(
			nr.GraphWalkExpr(&expr.Expr),
			nr.GraphWalkExprs(expr.Indices),
		)
	case *tree.SliceExpr:
		return common.MergeSets(
			nr.GraphWalkExpr(&expr.Expr),
			nr.GraphWalkExpr(&expr.Low),
			nr.GraphWalkExpr(&expr.High),
			nr.GraphWalkExpr(&expr.Max),
		)
	case *tree.TypeSwitchAssertionExpr:
		return common.MergeSets(
			nr.GraphWalkExpr(&expr.Expr),
		)
	case *tree.TypeAssertionExpr:
		return common.MergeSets(
			nr.GraphWalkExpr(&expr.Expr),
			nr.GraphWalkType(&expr.Type),
		)
	case *tree.CallExpr:
		allDeps := make([]Deps, len(expr.Args))
		for i, arg := range expr.Args {
			deps := nr.GraphWalkExpr(&arg)
			allDeps[i] = deps
		}
		return common.MergeSets(allDeps...)
	case *tree.NameExpr:
		return NewVarRef(arg)
	case *tree.LiteralExpr:
		return NoDeps
	case *tree.FuncLitExpr:
		return nr.GraphWalkSignature(expr.Signature)
	case *tree.CompositeLitExpr:
		allDeps := make([]Deps, len(expr.Elems))
		for i, elem := range expr.Elems {
			deps := common.MergeSets(
				nr.GraphWalkExpr(&elem.Key),
				nr.GraphWalkExpr(&elem.Value),
			)
			allDeps[i] = deps
		}
		return common.MergeSets(allDeps...)
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func (nr *NameResolver) GraphWalkExprs(exprs []tree.Expr) Deps {
	allDeps := make([]Deps, len(exprs))
	for i := range exprs {
		deps := nr.GraphWalkExpr(&exprs[i])
		allDeps[i] = deps
	}
	return common.MergeSets(allDeps...)
}

// ====================

func (nr *NameResolver) CheckCycles() {
	// only top-level VarDecls & ConstDecls can have cycles
	// all other declarations are part of an implicit LET REC
	var checking = make(map[common.Identifier]*NameInfo)
	for id, name := range nr.Context.Names {
		if name.Scope == ScopeKindPackage && (name.Kind == common.DeclKindVar || name.Kind == common.DeclKindConst) {
			checking[id] = name
		}
	}

	cycle := algos.FindCycle(checking, func(name *NameInfo) common.Set[common.Identifier] {
		if name == nil {
			return common.Set[common.Identifier]{}
		}
		deps := common.Set[common.Identifier]{}
		for ref := range name.Dependencies {
			if ref.Expr != nil {
				deps.Add((*ref.Expr).(*tree.NameExpr).Name)
			}
			if ref.Type != nil {
				deps.Add((*ref.Type).(*tree.TypeName).Name)
			}
		}
		return deps
	})

	if len(cycle) > 0 {
		panic(fmt.Sprintf("cycle in dependencies: %v", cycle))
	}
}

// ====================

func (nr *NameResolver) BindContext(ctx *NameContext, reps map[common.Identifier]tree.Node) {
	for _, name := range ctx.Names {
		reps[name.Name] = nr.MakeReplacement(name)
	}
	for _, name := range ctx.Names {
		for ref := range name.Dependencies {
			if ref.Expr != nil {
				target, ok := (*ref.Expr).(*tree.NameExpr)
				if !ok {
					continue
				}
				rep := reps[target.Name]
				if rep == nil {
					continue
				}
				switch rep := rep.(type) {
				case tree.Expr:
					*ref.Expr = rep
				case tree.Type:
					*ref.Expr = &tree.TypeExpr{Type: rep}
				default:
					panic("unreachable")
				}
			}
			if ref.Type != nil {
				target, ok := (*ref.Type).(*tree.TypeName)
				if !ok {
					continue
				}
				rep := reps[target.Name]
				if rep == nil {
					continue
				}
				*ref.Type = rep.(tree.Type)
			}
		}
	}
	for _, child := range ctx.Children {
		nr.BindContext(child, reps)
	}
}

func (nr *NameResolver) MakeReplacement(name *NameInfo) tree.Node {
	switch name.Kind {
	case common.DeclKindImport:
		// only present in expressions
		return &tree.ImportNameExpr{Path: nr.Imports[name.Name], Name: name.Name}
	case common.DeclKindConst, common.DeclKindVar, common.DeclKindFunc:
		if name.Scope == ScopeKindPackage {
			return &tree.PackageNameExpr{Path: nr.PackagePath, Name: name.Name}
		} else {
			return &tree.NameExpr{Name: name.Name}
		}
	case common.DeclKindType, common.DeclKindAlias:
		if name.Scope == ScopeKindPackage {
			return &tree.PackageTypeName{Path: nr.PackagePath, Name: name.Name}
		} else {
			return &tree.TypeName{Name: name.Name}
		}
	case common.DeclKindTypeParam:
		return &tree.TypeName{Name: name.Name}
	case common.DeclKindParam:
		return &tree.NameExpr{Name: name.Name}
	default:
		spew.Dump(name.Kind)
		panic("unreachable")
	}
}

// ====================
