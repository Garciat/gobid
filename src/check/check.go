package check

import (
	"fmt"
	"github.com/garciat/gobid/source"
	"reflect"
	"strconv"
	"strings"
	"sync"

	"github.com/davecgh/go-spew/spew"

	. "github.com/garciat/gobid/common"
	"github.com/garciat/gobid/tree"
)

type Checker struct {
	Fresh *int

	Packages       map[ImportPath]*source.Package
	PackageSymbols map[ImportPath]*VarContext

	TyCtx    *TypeContext
	VarCtx   *VarContext
	Builtins *VarContext

	CurFn *tree.Signature
	CurTy *tree.TypeDecl
}

func NewChecker(packages map[ImportPath]*source.Package) *Checker {
	return &Checker{
		Fresh:    Ptr(0),
		Packages: packages,
		PackageSymbols: map[ImportPath]*VarContext{
			"unsafe": MakeUnsafePackage(),
		},
		TyCtx:    &TypeContext{},
		VarCtx:   NewVarContext(),
		Builtins: MakeBuiltins(),
	}
}

func (c *Checker) Copy() *Checker {
	return &Checker{
		Fresh:          c.Fresh,
		Packages:       c.Packages,
		PackageSymbols: c.PackageSymbols,
		TyCtx:          c.TyCtx,
		VarCtx:         c.VarCtx,
		Builtins:       c.Builtins,
		CurFn:          c.CurFn,
		CurTy:          c.CurTy,
	}
}

func (c *Checker) Run() {
	packages := SortPackages(c.Packages)

	packageScopes := make(map[*source.Package]*Checker, len(packages))
	fileScopes := make(map[*source.FileDef]*Checker, len(packages)) // minimum size :shrug:

	for _, pkg := range packages {
		if pkg.ImportPath == "unsafe" {
			continue // TODO should not be here
		}

		packageScopes[pkg] = c.BeginScope(ScopeKindPackage)
		for _, file := range pkg.Files {
			fileScopes[file] = packageScopes[pkg].BeginScope(ScopeKindFile)

			for _, decl := range file.Decls {
				switch decl := decl.(type) {
				case *tree.ImportDecl:
					fileScopes[file].DefineImport(decl)
				}
			}
		}

		c.PackageSymbols[pkg.ImportPath] = packageScopes[pkg].VarCtx // TODO seems unprincipled
	}

	packageSortedDecls := make(map[*source.Package][]tree.Decl, len(packages))

	for _, pkg := range packages {
		if pkg.ImportPath == "unsafe" {
			continue // TODO should not be here
		}

		packageSortedDecls[pkg] = ResolvePackageNames(pkg)
	}

	for _, pkg := range packages {
		if pkg.ImportPath == "unsafe" {
			continue // TODO should not be here
		}

		// Keep track which file owns which declaration
		declFile := map[tree.Decl]*source.FileDef{}
		for _, file := range pkg.Files {
			for _, decl := range file.Decls {
				declFile[decl] = file
			}
		}

		fmt.Printf("=== Loading Package (%v) ===\n", pkg.ImportPath)
		for _, decl := range packageSortedDecls[pkg] {
			switch decl := decl.(type) {
			case *tree.ImportDecl:
				// done earlier
			default:
				fmt.Println(declFile[decl].Path)
				scope := fileScopes[declFile[decl]]
				scope.DefineTopLevelDecl(decl)
			}
		}
	}

	for _, pkg := range packages {
		fmt.Printf("=== Checking Package (%v) ===\n", pkg.ImportPath)
		for _, file := range pkg.Files {
			scope := fileScopes[file]
			scope.CheckFile(file)
		}
	}
}

func (c *Checker) LoadFile(file *source.FileDef) func() {
	fmt.Printf("=== Checker.LoadFile(%v) ===\n", file.Path)
	later := []tree.Decl{}
	for _, decl := range file.Decls {
		switch decl.(type) {
		case *tree.VarDecl, *tree.ConstDecl:
			later = append(later, decl)
		default:
			c.DefineDecl(decl)
		}
	}
	return func() {
		for _, decl := range later {
			c.DefineDecl(decl)
		}
	}
}

func (c *Checker) CheckFile(file *source.FileDef) {
	fmt.Printf("=== Checker.CheckFile(%v) ===\n", file.Path)
	for _, decl := range file.Decls {
		c.CheckDecl(decl)
	}
}

// ========================

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

func (c *Checker) DefineConstant(name Identifier, ty tree.Type, expr tree.Expr) {
	fmt.Printf("DEFINING %v = %v\n", name, ty)

	var target *VarContext
	if c.VarCtx.ScopeKind == ScopeKindFile {
		Assert(c.VarCtx.Parent.ScopeKind == ScopeKindPackage, "expected package scope")
		target = c.VarCtx.Parent
	} else {
		target = c.VarCtx
	}

	target.DefConst(name, ty, expr)
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
	fmt.Printf("DEFINING func (%s%v) %v%v\n", prefix, holder, name, ty.Type.Signature)
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

// ========================

func (c *Checker) ResolveValue(ty tree.Type) tree.Type {
	switch ty := ty.(type) {
	case *tree.TypeName:
		return c.Lookup(ty.Name)
	case *tree.PackageTypeName:
		return c.PackageLookup(ty.Path, ty.Name)
	case *tree.QualIdentifier:
		// TODO hacky
		if ty.Package == "" {
			return c.Lookup(ty.Name)
		}
		imp, ok := c.Lookup(NewIdentifier(ty.Package)).(*tree.ImportType)
		if !ok {
			panic(fmt.Errorf("not an import: %v", ty.Package))
		}
		return c.PackageLookup(imp.ImportPath, ty.Name)
	default:
		return ty
	}
}

func (c *Checker) ResolveType(ty tree.Type) tree.Type {
	var valueTy tree.Type
	switch ty := ty.(type) {
	case *tree.TypeName:
		valueTy = c.ResolveValue(ty)
	case *tree.PackageTypeName:
		valueTy = c.ResolveValue(ty)
	case *tree.QualIdentifier:
		valueTy = c.ResolveValue(ty)
	default:
		return ty
	}

	switch valueTy := valueTy.(type) {
	case *tree.TypeOfType:
		return valueTy.Type
	default:
		panic(fmt.Errorf("not a type: %v", ty))
	}
}

func (c *Checker) CheckAssignableTo(src, dst tree.Type) {
	c.TyCtx.AddRelation(RelationSubtype{Sub: src, Super: dst})
}

// ========================

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

func (c *Checker) BeginScope(kind ScopeKind) *Checker {
	copy := c.Copy()
	copy.TyCtx = c.TyCtx.Fork()
	copy.VarCtx = c.VarCtx.Fork(kind)
	return copy
}

func (c *Checker) BeginTypeScope(decl *tree.TypeDecl) *Checker {
	copy := c.BeginScope(ScopeKindType)
	copy.CurTy = decl
	return copy
}

func (c *Checker) BeginFunctionScope(fn *tree.Signature) *Checker {
	copy := c.BeginScope(ScopeKindFunction)
	copy.CurFn = fn
	return copy
}

func (c *Checker) AssertInFunctionScope() *tree.Signature {
	if c.CurFn == nil {
		panic("not in function scope")
	}
	return c.CurFn
}

// ========================

func (c *Checker) DefineTopLevelDecl(decl tree.Decl) {
	switch decl := decl.(type) {
	case *tree.ImportDecl:
		c.DefineImportDecl(decl)
	case *tree.ConstDecl:
		c.DefineConstDecl(decl)
	case *tree.TypeDecl:
		c.DefineTypeDecl(decl)
	case *tree.AliasDecl:
		c.DefineAliasDecl(decl)
	case *tree.VarDecl:
		c.DefineVarDecl(decl)
	case *tree.FunctionDecl:
		c.DefineFunctionDecl(decl)
	case *tree.MethodDecl:
		c.DefineMethodDecl(decl)
	default:
		spew.Dump(decl)
		panic("unreachable")
	}
}

func (c *Checker) DefineDecl(decl tree.Decl) {
	switch decl := decl.(type) {
	case *tree.ConstDecl:
		c.DefineConstDecl(decl)
	case *tree.TypeDecl:
		c.DefineTypeDecl(decl)
	case *tree.AliasDecl:
		c.DefineAliasDecl(decl)
	case *tree.VarDecl:
		c.DefineVarDecl(decl)
	default:
		spew.Dump(decl)
		panic("unreachable")
	}
}

func (c *Checker) DefineImportDecl(decl *tree.ImportDecl) {
	c.DefineImport(decl)
}

func (c *Checker) DefineConstDecl(decl *tree.ConstDecl) {
	spew.Dump(decl)
	var exprTy tree.Type = c.Synth(decl.Value)
	var declTy tree.Type
	if decl.Type != nil {
		declTy = c.ResolveType(decl.Type)
		c.CheckAssignableTo(exprTy, declTy)
	} else {
		declTy = exprTy
	}
	c.DefineConstant(decl.Name, declTy, decl.Value)
}

func (c *Checker) DefineTypeDecl(decl *tree.TypeDecl) {
	var ty tree.Type
	if len(decl.TypeParams.Params) > 0 {
		ty = &tree.NamedType{Name: decl.Name, Type: &tree.GenericType{TypeParams: decl.TypeParams, Type: decl.Type}}
	} else {
		ty = &tree.NamedType{Name: decl.Name, Type: decl.Type}
	}
	c.DefineType(decl.Name, ty)
}

func (c *Checker) DefineAliasDecl(decl *tree.AliasDecl) {
	c.DefineType(decl.Name, decl.Type)
}

func (c *Checker) DefineVarDecl(decl *tree.VarDecl) {
	if len(decl.Names) == 1 && decl.Expr != nil {
		for _, name := range decl.Names {
			ty := c.Synth(decl.Expr)
			if _, ok := ty.(*tree.TupleType); ok {
				panic("cannot use tuple as value")
			}
			if decl.Type != nil {
				declTy := c.ResolveType(decl.Type)
				c.CheckAssignableTo(ty, declTy)
				c.DefineValue(name, declTy)
			} else {
				c.DefineValue(name, ty)
			}
		}
	} else if len(decl.Names) > 1 && decl.Expr != nil {
		tupleTy := sync.OnceValue(func() *tree.TupleType {
			exprTy := c.Synth(decl.Expr)
			tupleTy, ok := exprTy.(*tree.TupleType)
			if !ok {
				panic("multiple-value return in single-value context")
			}
			if len(tupleTy.Elems) != len(decl.Names) {
				panic(fmt.Sprintf("assignment mismatch: %v variables but RHS returns %v values", len(decl.Names), len(tupleTy.Elems)))
			}
			return tupleTy
		})
		for i, name := range decl.Names {
			elemTy := c.ResolveType(tupleTy().Elems[i])
			if decl.Type != nil {
				declTy := c.ResolveType(decl.Type)
				c.CheckAssignableTo(elemTy, declTy)
				c.DefineValue(name, declTy)
			} else {
				c.DefineValue(name, elemTy)
			}
		}
	} else if len(decl.Names) > 0 && decl.Expr == nil {
		if decl.Type == nil {
			panic("variable declaration without type or initializing expression")
		}
		for _, name := range decl.Names {
			c.DefineValue(name, decl.Type)
		}
	} else {
		spew.Dump(decl)
		panic("unreachable?")
	}
}

func (c *Checker) DefineFunctionDecl(decl *tree.FunctionDecl) {
	c.DefineFunction(decl.Name, &tree.FunctionType{Signature: decl.Signature})
}

func (c *Checker) DefineMethodDecl(decl *tree.MethodDecl) {
	pointerReceiver := false

	receiverTy := decl.Receiver.Type
	if pointerTy, ok := receiverTy.(*tree.PointerType); ok {
		receiverTy = pointerTy.BaseType
		pointerReceiver = true
	}

	var methodHolder Identifier

	switch receiverTy := receiverTy.(type) {
	case *tree.TypeName:
		methodHolder = receiverTy.Name
	case *tree.TypeApplication:
		switch receiverTy := receiverTy.Type.(type) {
		case *tree.TypeName:
			methodHolder = receiverTy.Name
		case *tree.QualIdentifier:
			panic("cannot have package-qualified receiver type")
		default:
			panic("unreachable?")
		}
	default:
		spew.Dump(receiverTy)
		panic("TODO")
	}

	methodTy := &tree.MethodType{
		PointerReceiver: pointerReceiver,
		Type:            &tree.FunctionType{Signature: decl.Signature},
	}

	c.DefineMethod(methodHolder, decl.Name, methodTy)
}

// ========================

func (c *Checker) CheckDecl(decl tree.Decl) {
	switch decl := decl.(type) {
	case *tree.ImportDecl:
		c.CheckImportDecl(decl)
	case *tree.ConstDecl:
		c.CheckConstDecl(decl)
	case *tree.TypeDecl:
		c.CheckTypeDecl(decl)
	case *tree.AliasDecl:
		c.CheckAliasDecl(decl)
	case *tree.VarDecl:
		c.CheckVarDecl(decl)
	case *tree.FunctionDecl:
		c.CheckFunctionDecl(decl)
	case *tree.MethodDecl:
		c.CheckMethodDecl(decl)
	default:
		spew.Dump(decl)
		panic("unreachable")
	}
}

func (c *Checker) CheckImportDecl(decl *tree.ImportDecl) {
	// nothing to do
}

func (c *Checker) CheckConstDecl(decl *tree.ConstDecl) {
	// TODO: check constant expressions?
}

func (c *Checker) CheckTypeDecl(decl *tree.TypeDecl) {
	fmt.Printf("=== CheckTypeDecl(%v) ===\n", decl.Name)

	scope := c.BeginTypeScope(decl)

	for _, tyParam := range decl.TypeParams.Params {
		scope.DefineType(tyParam.Name, &tree.TypeParam{Name: tyParam.Name, Bound: tyParam.Constraint})
	}

	scope.CheckTypeDeclType(c.ResolveType(decl.Type))

	subst := scope.Verify()

	scope.CheckSubst(decl.TypeParams, subst)
}

func (c *Checker) CheckTypeDeclType(ty tree.Type) {
	switch ty := ty.(type) {
	case *tree.TypeBuiltin:
		// nothing to do
	case *tree.TypeParam:
		// nothing to do
	case *tree.TypeApplication:
		c.TypeApplicationFunc(ty, func(tyParam *tree.TypeParamDecl, tyArg tree.Type) {
			c.CheckTypeDeclType(c.ResolveType(tyArg))
		})
	case *tree.StructType:
		for _, field := range ty.Fields {
			c.CheckTypeDeclType(c.ResolveType(field.Type))
		}
	case *tree.InterfaceType:
		for _, m := range ty.Methods {
			c.CheckTypeDeclType(m.Type)
		}
		for _, ctr := range ty.Constraints {
			for _, term := range ctr.TypeElem.Union {
				c.CheckTypeDeclType(c.ResolveType(term.Type))
			}
		}
	case *tree.FunctionType:
		c.CheckTypeDeclSignature(ty.Signature)
	case *tree.SliceType:
		c.CheckTypeDeclType(c.ResolveType(ty.ElemType))
	case *tree.NamedType:
		c.CheckTypeDeclType(c.ResolveType(ty.Type))
	case *tree.PointerType:
		c.CheckTypeDeclType(c.ResolveType(ty.BaseType))
	case *tree.ArrayType:
		c.CheckTypeDeclType(c.ResolveType(ty.ElemType))
	case *tree.MapType:
		c.CheckTypeDeclType(c.ResolveType(ty.KeyType))
		c.CheckTypeDeclType(c.ResolveType(ty.ElemType))
	default:
		spew.Dump(ty)
		panic("unreachable")
	}
}

func (c *Checker) CheckTypeDeclSignature(sig *tree.Signature) {
	if len(sig.TypeParams.Params) > 0 {
		panic("function type with type parameters")
	}
	for _, param := range sig.Params.Params {
		c.CheckTypeDeclType(c.ResolveType(param.Type))
	}
	for _, result := range sig.Results.Params {
		c.CheckTypeDeclType(c.ResolveType(result.Type))
	}
}

func (c *Checker) CheckAliasDecl(decl *tree.AliasDecl) {
	// nothing to do
}

func (c *Checker) CheckVarDecl(decl *tree.VarDecl) {
	// nothing to do?
}

func (c *Checker) CheckFunctionDecl(decl *tree.FunctionDecl) {
	fmt.Printf("=== CheckFunctionDecl(%v) ===\n", decl.Name)

	scope := c.BeginFunctionScope(decl.Signature)

	scope.DoFunctionDecl(decl.Signature, decl.Body)
}

func (c *Checker) CheckMethodDecl(decl *tree.MethodDecl) {
	fmt.Printf("=== CheckMethodDecl(%v) ===\n", decl.Name)

	scope := c.BeginFunctionScope(decl.Signature)

	var receiverTy tree.Type = scope.ResolveType(decl.Receiver.Type)

	if pointerTy, ok := receiverTy.(*tree.PointerType); ok {
		receiverTy = scope.ResolveType(pointerTy.BaseType)
	}

	switch ty := receiverTy.(type) {
	case *tree.NamedType:
		// nothing to do
	case *tree.TypeApplication:
		named, ok := scope.ResolveType(ty.Type).(*tree.NamedType)
		if !ok {
			panic("method on non-named type")
		}
		gen, ok := named.Type.(*tree.GenericType)
		if !ok {
			panic("not a generic type")
		}
		if len(gen.TypeParams.Params) != len(ty.Args) {
			panic("wrong number of type arguments")
		}
		for _, tyParam := range gen.TypeParams.Params {
			scope.DefineType(tyParam.Name, &tree.TypeParam{Name: tyParam.Name, Bound: tyParam.Constraint})
		}
	default:
		spew.Dump(ty)
		panic("method on non-named type")
	}

	scope.DefineValue(decl.Receiver.Name, decl.Receiver.Type)

	scope.DoFunctionDecl(decl.Signature, decl.Body)
}

func (c *Checker) DoFunctionDecl(sig *tree.Signature, stmts *tree.StatementList) {
	for _, tyParam := range sig.TypeParams.Params {
		c.DefineType(tyParam.Name, &tree.TypeParam{Name: tyParam.Name, Bound: tyParam.Constraint})
	}

	for _, param := range sig.Params.Params {
		if param.Variadic {
			c.DefineValue(param.Name, &tree.SliceType{ElemType: c.ResolveType(param.Type)})
		} else {
			c.DefineValue(param.Name, c.ResolveType(param.Type))
		}
	}

	for _, result := range sig.Results.Params {
		c.DefineValue(result.Name, c.ResolveType(result.Type))
	}

	c.CheckStatementList(stmts)

	subst := c.Verify()
	c.CheckSubst(sig.TypeParams, subst)
}

func (c *Checker) CheckStatementList(list *tree.StatementList) {
	for _, stmt := range list.Stmts {
		c.CheckStatement(stmt)
	}
}

func (c *Checker) CheckStatement(stmt tree.Statement) {
	switch stmt := stmt.(type) {
	case *tree.DeclStmt:
		for _, decl := range stmt.Decls {
			c.DefineDecl(decl)
			c.CheckDecl(decl)
		}
	case *tree.ExpressionStmt:
		c.Synth(stmt.Expr) // ???
	case *tree.EmptyStmt:
		// do nothing
	case *tree.ReturnStmt:
		c.CheckReturnStmt(stmt)
	case *tree.IfStmt:
		c.CheckIfStmt(stmt)
	case *tree.ShortVarDecl:
		c.CheckShortVarDecl(stmt)
	case *tree.RangeStmt:
		c.CheckRangeStmt(stmt)
	case *tree.IncDecStmt:
		c.CheckIncDecStmt(stmt)
	case *tree.AssignmentStmt:
		c.CheckAssignmentStmt(stmt)
	case *tree.BranchStmt:
		// nothing?
	case *tree.ForStmt:
		c.CheckForStmt(stmt)
	case *tree.SwitchStmt:
		c.CheckSwitchStmt(stmt)
	default:
		spew.Dump(stmt)
		panic("unreachable")
	}
}

func (c *Checker) CheckShortVarDecl(stmt *tree.ShortVarDecl) {
	if len(stmt.Names) == len(stmt.Exprs) {
		for i, name := range stmt.Names {
			ty := c.Synth(stmt.Exprs[i])
			c.DefineValue(name, ty)
		}
	} else if len(stmt.Names) > 1 && len(stmt.Exprs) == 1 {
		ty := c.Synth(stmt.Exprs[0])
		switch ty := c.Under(ty).(type) {
		case *tree.TupleType:
			if len(stmt.Names) != len(ty.Elems) {
				panic("wrong number of return in tuple expansion")
			}
			for i, name := range stmt.Names {
				c.DefineValue(name, ty.Elems[i])
			}
		default:
			panic("non-tuple type")
		}
	} else {
		panic("wrong number of expressions")
	}
}

func (c *Checker) CheckAssignmentStmt(stmt *tree.AssignmentStmt) {
	if len(stmt.LHS) == len(stmt.RHS) {
		for i := range stmt.LHS {
			if tree.IsIgnoreName(stmt.LHS[i]) {
				continue
			}
			c.CheckAssignableTo(c.Synth(stmt.RHS[i]), c.Synth(stmt.LHS[i]))
		}
	} else if len(stmt.LHS) > 1 && len(stmt.RHS) == 1 {
		ty := c.Synth(stmt.RHS[0])
		switch ty := c.Under(ty).(type) {
		case *tree.TupleType:
			if len(stmt.LHS) != len(ty.Elems) {
				panic("wrong number of return in tuple expansion")
			}
			for i := range stmt.LHS {
				if tree.IsIgnoreName(stmt.LHS[i]) {
					continue
				}
				c.CheckAssignableTo(c.ResolveType(ty.Elems[i]), c.Synth(stmt.LHS[i]))
			}
		default:
			panic("non-tuple type")
		}
	} else {
		panic("wrong number of expressions")
	}
}

func (c *Checker) CheckReturnStmt(stmt *tree.ReturnStmt) {
	fn := c.AssertInFunctionScope()
	if len(stmt.Results) == len(fn.Results.Params) {
		for i, result := range stmt.Results {
			ty := c.Synth(result)
			c.CheckAssignableTo(ty, fn.Results.Params[i].Type)
		}
	} else if len(stmt.Results) == 1 && len(fn.Results.Params) > 0 {
		ty := c.Synth(stmt.Results[0])
		switch ty := c.Under(ty).(type) {
		case *tree.TupleType:
			if len(fn.Results.Params) != len(ty.Elems) {
				panic("wrong number of return in tuple expansion")
			}
			for i, param := range fn.Results.Params {
				c.CheckAssignableTo(ty.Elems[i], param.Type)
			}
		default:
			panic("non-tuple type")
		}
	} else if len(stmt.Results) == 0 && fn.HasNamedResults() {
		return
	} else {
		panic("wrong number of expressions in return")
	}
}

func (c *Checker) CheckIfStmt(stmt *tree.IfStmt) {
	if stmt.Init != nil {
		c.CheckStatement(stmt.Init)
	}
	if stmt.Cond != nil {
		// TODO only allowed for else
		c.CheckExpr(stmt.Cond, c.BuiltinType("bool"))
	}
	c.CheckStatementList(stmt.Body)
	if stmt.Else != nil {
		c.CheckStatement(stmt.Else)
	}
}

func (c *Checker) CheckIncDecStmt(stmt *tree.IncDecStmt) {
	exprTy := c.Synth(stmt.Expr)
	exprTy = c.ResolveType(exprTy)
	if !c.IsNumeric(exprTy) {
		spew.Dump(exprTy)
		panic("non-numeric type")
	}
	// TODO emit relation instead of greedy check?
}

func (c *Checker) CheckRangeStmt(stmt *tree.RangeStmt) {
	scope := c.BeginScope(ScopeKindBlock)

	targetTy := scope.Synth(stmt.X)

	var keyTy, valueTy tree.Type

	switch targetTy := scope.Under(targetTy).(type) {
	case *tree.SliceType:
		keyTy = scope.BuiltinType("int")
		valueTy = scope.ResolveType(targetTy.ElemType)
	case *tree.MapType:
		keyTy = scope.ResolveType(targetTy.KeyType)
		valueTy = scope.ResolveType(targetTy.ElemType)
	case *tree.ArrayType:
		keyTy = scope.BuiltinType("int")
		valueTy = scope.ResolveType(targetTy.ElemType)
	}

	if keyTy == nil || valueTy == nil {
		panic(fmt.Sprintf("cannot range over %v", targetTy))
	}

	if stmt.Key != nil {
		if stmt.Assign {
			scope.DefineValue(stmt.Key.(*tree.NameExpr).Name, keyTy)
		} else {
			scope.CheckAssignableTo(keyTy, scope.Synth(stmt.Key))
		}
	}
	if stmt.Value != nil {
		if stmt.Assign {
			scope.DefineValue(stmt.Value.(*tree.NameExpr).Name, valueTy)
		} else {
			scope.CheckAssignableTo(valueTy, scope.Synth(stmt.Value))
		}
	}

	scope.CheckStatementList(stmt.Body)
}

func (c *Checker) CheckForStmt(stmt *tree.ForStmt) {
	scope := c.BeginScope(ScopeKindBlock)
	if stmt.Init != nil {
		scope.CheckStatement(stmt.Init)
	}
	if stmt.Cond != nil {
		scope.CheckExpr(stmt.Cond, c.BuiltinType("bool"))
	}
	if stmt.Post != nil {
		scope.CheckStatement(stmt.Post)
	}
	scope.CheckStatementList(stmt.Body)
}

func (c *Checker) CheckSwitchStmt(stmt *tree.SwitchStmt) {
	scope := c.BeginScope(ScopeKindBlock)
	var tagTy tree.Type
	if stmt.Init != nil {
		scope.CheckStatement(stmt.Init)
	}
	if stmt.Tag != nil {
		tagTy = scope.Synth(stmt.Tag)
	}
	for _, caseStmt := range stmt.Cases {
		for _, expr := range caseStmt.Exprs {
			if tagTy != nil {
				scope.CheckExpr(expr, tagTy)
			} else {
				scope.CheckExpr(expr, scope.BuiltinType("bool"))
			}
		}
		scope.CheckStatementList(caseStmt.Body)
	}
}

// ========================

type TypeSet struct {
	Methods  []*tree.MethodElem
	Types    []tree.Type
	Universe bool
}

func (c *Checker) Combine(lhs, rhs TypeSet) TypeSet {
	result := TypeSet{
		Methods:  []*tree.MethodElem{},
		Types:    []tree.Type{},
		Universe: lhs.Universe && rhs.Universe,
	}

	// combine
	copy(result.Methods, lhs.Methods)

	for _, m := range rhs.Methods {
		for _, n := range lhs.Methods {
			if m.Name == n.Name {
				if !c.Identical(m.Type, n.Type) {
					panic("method clash")
				}
				continue
			}
		}
		result.Methods = append(result.Methods, m)
	}

	if lhs.Universe && rhs.Universe {
		if len(lhs.Types) != 0 {
			panic("weird")
		}
		if len(rhs.Types) != 0 {
			panic("weird")
		}
	} else if lhs.Universe && !rhs.Universe {
		result.Types = append(result.Types, rhs.Types...)
	} else if !lhs.Universe && rhs.Universe {
		result.Types = append(result.Types, lhs.Types...)
	} else {
		// intersect
		for _, t := range rhs.Types {
			for _, u := range lhs.Types {
				if c.Identical(t, u) {
					result.Types = append(result.Types, t)
				}
			}
		}
	}

	return result
}

func (c *Checker) CheckSubst(tyParams *tree.TypeParamList, subst Subst) {
	for _, tyParam := range tyParams.Params {
		tySub, ok := subst[tyParam.Name]
		if !ok {
			continue // TODO: is this ok?
		}
		if single, ok := IsSingleTypeUnion(tyParam.Constraint); ok {
			if c.Identical(tySub, c.ResolveType(single)) {
				continue
			}
		}
		panic(fmt.Sprintf("type param %v with constraint %v cannot be %v", tyParam.Name, tyParam.Constraint, tySub))
	}
}

// ========================

func (c *Checker) IsNumeric(ty tree.Type) bool {
	switch ty := c.Under(ty).(type) {
	case *tree.TypeBuiltin:
		return ty.IsNumeric
	case *tree.UntypedConstantType:
		return ty.IsNumeric()
	default:
		return false
	}
}

// ========================

func (c *Checker) CheckExpr(expr tree.Expr, ty tree.Type) {
	switch expr := expr.(type) {
	case *tree.NameExpr:
		c.CheckNameExpr(expr, ty)
	case *tree.PackageNameExpr:
		c.CheckPackageNameExpr(expr, ty)
	case *tree.BinaryExpr:
		c.CheckBinaryExpr(expr, ty)
	case *tree.UnaryExpr:
		c.CheckUnaryExpr(expr, ty)
	case *tree.ConversionExpr:
		panic("TODO")
	case *tree.SelectorExpr:
		c.CheckSelectorExpr(expr, ty)
	case *tree.IndexExpr:
		c.CheckIndexExpr(expr, ty)
	case *tree.TypeAssertionExpr:
		panic("TODO")
	case *tree.CallExpr:
		c.CheckCallExpr(expr, ty)
	case *tree.LiteralExpr:
		c.CheckLiteralExpr(expr, ty)
	case *tree.CompositeLitExpr:
		c.CheckCompositeLitExpr(expr, ty)
	case *tree.FuncLitExpr:
		c.CheckFuncLitExpr(expr, ty)
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func (c *Checker) CheckNameExpr(expr *tree.NameExpr, ty tree.Type) {
	c.TyCtx.AddEq(c.Lookup(expr.Name), c.ResolveType(ty))
}

func (c *Checker) CheckPackageNameExpr(expr *tree.PackageNameExpr, ty tree.Type) {
	c.TyCtx.AddEq(c.Synth(expr), c.ResolveType(ty))
}

func (c *Checker) CheckBinaryExpr(expr *tree.BinaryExpr, ty tree.Type) {
	c.CheckAssignableTo(c.Synth(expr), c.ResolveType(ty))
}

func (c *Checker) CheckUnaryExpr(expr *tree.UnaryExpr, ty tree.Type) {
	exprTy := c.Synth(expr.Expr)
	switch expr.Op {
	case tree.UnaryOpNeg:
		// TODO check is numeric
		c.CheckExpr(expr.Expr, c.ResolveType(ty))
	case tree.UnaryOpNot:
		c.CheckExpr(expr.Expr, c.BuiltinType("bool"))
	case tree.UnaryOpAddr:
		c.CheckAssignableTo(&tree.PointerType{BaseType: exprTy}, c.ResolveType(ty))
	case tree.UnaryOpDeref:
		switch exprTy := c.Under(exprTy).(type) {
		case *tree.PointerType:
			c.CheckAssignableTo(c.ResolveType(exprTy.BaseType), c.ResolveType(ty))
		default:
			spew.Dump(expr)
			panic("cannot dereference non-pointer")
		}
	default:
		spew.Dump(expr, ty)
		panic("unreachable")
	}
}

func (c *Checker) CheckSelectorExpr(expr *tree.SelectorExpr, ty tree.Type) {
	c.CheckAssignableTo(c.Synth(expr), c.ResolveType(ty))
}

func (c *Checker) CheckIndexExpr(expr *tree.IndexExpr, ty tree.Type) {
	switch exprTy := c.Synth(expr.Expr).(type) {
	case *tree.SliceType:
		if len(expr.Indices) != 1 {
			panic("indexing a slice with multiple indices")
		}
		c.CheckExpr(expr.Indices[0], tree.UntypedInt())
		c.TyCtx.AddEq(c.ResolveType(exprTy.ElemType), c.ResolveType(ty))
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func (c *Checker) CheckCallExpr(expr *tree.CallExpr, ty tree.Type) {
	callTy := c.Synth(expr)
	c.CheckAssignableTo(callTy, c.ResolveType(ty))
}

func (c *Checker) CheckLiteralExpr(expr *tree.LiteralExpr, ty tree.Type) {
	exprTy := c.Synth(expr)
	c.CheckAssignableTo(exprTy, c.ResolveType(ty))
}

func (c *Checker) CheckCompositeLitExpr(expr *tree.CompositeLitExpr, ty tree.Type) {
	if expr.Type == nil {
		c.MakeCompositeLit(expr, c.ResolveType(ty))
	} else {
		exprTy := c.Synth(expr)
		c.CheckAssignableTo(exprTy, c.ResolveType(ty))
	}
}

func (c *Checker) CheckFuncLitExpr(expr *tree.FuncLitExpr, ty tree.Type) {
	c.CheckAssignableTo(c.Synth(expr), c.ResolveType(ty))
}

// ========================

func (c *Checker) Synth(expr tree.Expr) tree.Type {
	switch expr := expr.(type) {
	case *tree.NameExpr:
		return c.SynthNameExpr(expr)
	case *tree.ImportNameExpr:
		return c.SynthImportNameExpr(expr)
	case *tree.PackageNameExpr:
		return c.SynthPackageNameExpr(expr)
	case *tree.BinaryExpr:
		return c.SynthBinaryExpr(expr)
	case *tree.UnaryExpr:
		return c.SynthUnaryExpr(expr)
	case *tree.ConversionExpr:
		return c.SynthConversionExpr(expr)
	case *tree.SelectorExpr:
		return c.SynthSelectorExpr(expr)
	case *tree.IndexExpr:
		return c.SynthIndexExpr(expr)
	case *tree.TypeAssertionExpr:
		panic("TODO")
	case *tree.CallExpr:
		return c.SynthCallExpr(expr)
	case *tree.LiteralExpr:
		return c.SynthLiteralExpr(expr)
	case *tree.TypeExpr:
		return &tree.TypeOfType{Type: c.ResolveType(expr.Type)}
	case *tree.CompositeLitExpr:
		return c.SynthCompositeLitExpr(expr)
	case *tree.SliceExpr:
		return c.SynthSliceExpr(expr)
	case *tree.FuncLitExpr:
		return c.SynthFuncLitExpr(expr)
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func (c *Checker) SynthNameExpr(expr *tree.NameExpr) tree.Type {
	return c.Lookup(expr.Name)
}

func (c *Checker) SynthImportNameExpr(expr *tree.ImportNameExpr) tree.Type {
	return &tree.ImportType{ImportPath: expr.Path}
}

func (c *Checker) SynthPackageNameExpr(expr *tree.PackageNameExpr) tree.Type {
	pkg, ok := c.PackageSymbols[expr.Path]
	if !ok {
		panic(fmt.Sprintf("package not loaded: %v", expr.Path))
	}

	ty, ok := pkg.Lookup(expr.Name)
	if !ok {
		panic(fmt.Sprintf("package %v has no symbol %v", expr.Path, expr.Name))
	}

	return ty
}

func (c *Checker) SynthBinaryExpr(expr *tree.BinaryExpr) tree.Type {
	switch expr.Op {
	case tree.BinaryOpEq, tree.BinaryOpNeq, tree.BinaryOpLt, tree.BinaryOpLte, tree.BinaryOpGt, tree.BinaryOpGte:
		// TODO check comparable
		leftTy := c.Synth(expr.Left)
		rightTy := c.Synth(expr.Right)
		c.TyCtx.AddEq(leftTy, rightTy)
		return c.BuiltinType("bool")
	case tree.BinaryOpAdd, tree.BinaryOpSub, tree.BinaryOpMul, tree.BinaryOpQuo, tree.BinaryOpRem:
		// TODO check numeric?
		leftTy := c.Synth(expr.Left)
		rightTy := c.Synth(expr.Right)
		c.TyCtx.AddEq(leftTy, rightTy)
		return leftTy
	case tree.BinaryOpLAnd, tree.BinaryOpLOr:
		c.CheckExpr(expr.Left, c.BuiltinType("bool"))
		c.CheckExpr(expr.Right, c.BuiltinType("bool"))
		return c.BuiltinType("bool")
	case tree.BinaryOpAnd, tree.BinaryOpOr, tree.BinaryOpXor, tree.BinaryOpAndNot, tree.BinaryOpShl, tree.BinaryOpShr:
		// TODO check numeric?
		leftTy := c.Synth(expr.Left)
		rightTy := c.Synth(expr.Right)
		c.TyCtx.AddEq(leftTy, rightTy)
		return leftTy
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func (c *Checker) SynthUnaryExpr(expr *tree.UnaryExpr) tree.Type {
	ty := c.Synth(expr.Expr)
	switch expr.Op {
	case tree.UnaryOpPos, tree.UnaryOpNeg:
		// TODO check numeric?
		return ty
	case tree.UnaryOpNot:
		c.CheckExpr(expr.Expr, c.BuiltinType("bool"))
		return c.BuiltinType("bool")
	case tree.UnaryOpAddr:
		return &tree.PointerType{BaseType: ty}
	case tree.UnaryOpDeref:
		switch ty := ty.(type) {
		case *tree.PointerType:
			return ty.BaseType
		case *tree.TypeOfType:
			return &tree.TypeOfType{Type: &tree.PointerType{BaseType: ty.Type}}
		default:
			spew.Dump(expr)
			panic(fmt.Sprintf("cannot dereference %v of type %v", expr, ty))
		}
	case tree.UnaryOpBitNot:
		// TODO check numeric?
		return ty
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func (c *Checker) SynthSelectorExpr(expr *tree.SelectorExpr) tree.Type {
	return c.DoSelect(c.Synth(expr.Expr), expr.Sel)
}

func (c *Checker) DoSelect(exprTy tree.Type, sel Identifier) tree.Type {
	switch imp := c.ResolveValue(exprTy).(type) {
	case *tree.ImportType:
		return c.PackageLookup(imp.ImportPath, sel)
	}

	checkTy := c.ResolveType(exprTy)

	switch ty := checkTy.(type) {
	case *tree.PointerType:
		checkTy = c.ResolveType(ty.BaseType)
	}

	switch ty := checkTy.(type) {
	case *tree.NamedType:
		for _, m := range c.NamedTypeMethods(ty) {
			if m.Name == sel {
				return m.Type
			}
		}
		checkTy = ty.Type
	case *tree.TypeParam:
		if ty.Bound != nil {
			set := c.InterfaceTypeSet(ty.Bound)
			for _, m := range set.Methods {
				if m.Name == sel {
					return m.Type
				}
			}
			if len(set.Types) == 1 {
				return c.DoSelect(c.ResolveType(set.Types[0]), sel)
			}
		}
	}

	switch ty := c.Under(checkTy).(type) {
	case *tree.StructType:
		for _, field := range ty.Fields {
			if field.Name == sel {
				return field.Type
			}
		}

	case *tree.InterfaceType:
		for _, m := range ty.Methods {
			if m.Name == sel {
				return m.Type
			}
		}
	}

	spew.Dump(c.Under(checkTy))
	panic(fmt.Sprintf("type %v has no field or method %v", exprTy, sel))
}

func (c *Checker) SynthIndexExpr(expr *tree.IndexExpr) tree.Type {
	exprTy := c.Synth(expr.Expr)

	switch ty := exprTy.(type) {
	case *tree.PointerType:
		exprTy = c.ResolveType(ty.BaseType)
	}

	var indexTy, resultTy tree.Type
	switch exprTy := c.Under(exprTy).(type) {
	case *tree.FunctionType:
		panic("unexpected function type (should be handled by CallExpr)")
	case *tree.SliceType:
		indexTy = tree.UntypedInt()
		resultTy = c.ResolveType(exprTy.ElemType)
	case *tree.MapType:
		indexTy = c.ResolveType(exprTy.KeyType)
		resultTy = c.ResolveType(exprTy.ElemType)
	case *tree.ArrayType:
		indexTy = tree.UntypedInt()
		resultTy = c.ResolveType(exprTy.ElemType)
	case *tree.TypeBuiltin:
		if exprTy.Name.Value == "string" {
			indexTy = tree.UntypedInt()
			resultTy = c.BuiltinType("byte")
		}
	case *tree.UntypedConstantType:
		if exprTy.IsCompatible("string") {
			indexTy = tree.UntypedInt()
			resultTy = c.BuiltinType("byte")
		}
	}

	if resultTy == nil || indexTy == nil {
		spew.Dump(reflect.TypeOf(exprTy))
		panic(fmt.Sprintf("cannot index type %v", exprTy))
	}

	return resultTy
}

func (c *Checker) SynthSliceExpr(expr *tree.SliceExpr) tree.Type {
	exprTy := c.Synth(expr.Expr)

	var resultTy tree.Type
	switch exprTy := exprTy.(type) {
	case *tree.SliceType:
		resultTy = exprTy
	case *tree.TypeBuiltin:
		if exprTy.Name.Value == "string" {
			resultTy = c.BuiltinType("string")
		}
	case *tree.ArrayType:
		resultTy = &tree.SliceType{ElemType: exprTy.ElemType}
	case *tree.UntypedConstantType:
		if exprTy.IsCompatible("string") {
			resultTy = c.BuiltinType("string")
		}
	}

	if resultTy == nil {
		spew.Dump(exprTy)
		panic(fmt.Sprintf("cannot slice type %v", exprTy))
	}

	if expr.Low != nil {
		c.CheckExpr(expr.Low, tree.UntypedInt())
	}
	if expr.High != nil {
		c.CheckExpr(expr.High, tree.UntypedInt())
	}
	if expr.Max != nil {
		c.CheckExpr(expr.Max, tree.UntypedInt())
	}

	return resultTy
}

func (c *Checker) SynthCallExpr(expr *tree.CallExpr) tree.Type {
	var funcTy *tree.FunctionType
	var typeArgs []tree.Type
	if index, ok := expr.Func.(*tree.IndexExpr); ok {
		if gen, ok := c.Synth(index.Expr).(*tree.FunctionType); ok {
			funcTy = gen
			for _, arg := range index.Indices {
				typeArgs = append(typeArgs, c.Synth(arg))
			}
		}
	}
	if funcTy == nil {
		switch ty := c.Synth(expr.Func).(type) {
		case *tree.FunctionType:
			funcTy = ty
		case *tree.BuiltinFunctionType:
			return c.SynthBuiltinFunctionCall(ty, expr)
		case *tree.SliceType:
			if len(expr.Args) != 1 {
				panic("conversion without exactly one argument")
			}
			switch elemType := c.Under(ty.ElemType).(type) {
			case *tree.TypeBuiltin:
				if elemType.Name.Value == "byte" {
					c.CheckAssignableTo(c.Synth(expr.Args[0]), c.BuiltinType("string"))
					return ty
				}
			}
			panic("TODO")
		case *tree.TypeBuiltin:
			if len(expr.Args) != 1 {
				panic("conversion without exactly one argument")
			}
			return c.SynthBuiltinConversion(expr.Args[0], ty)
		case *tree.TypeOfType:
			return c.SynthConversionExpr(&tree.ConversionExpr{Expr: expr.Func, Type: ty.Type})
		default:
			spew.Dump(ty)
			panic("not a function")
		}
	}
	var variadicParam *tree.ParameterDecl
	var variadicIndex int = -1
	if len(expr.Args) != len(funcTy.Signature.Params.Params) {
		variadicParam, variadicIndex = funcTy.Signature.GetVariadicParam()
		if variadicIndex != -1 && len(expr.Args) < len(funcTy.Signature.Params.Params) {
			panic("not enough arguments")
		}
	}
	if len(typeArgs) > len(funcTy.Signature.TypeParams.Params) {
		panic("too many type arguments")
	}

	subst := Subst{}
	for _, tyParam := range funcTy.Signature.TypeParams.Params {
		subst[tyParam.Name] = &tree.TypeParam{Name: c.FreshTypeName(), Bound: tyParam.Constraint}
	}
	funcTy = c.ApplySubst(funcTy, subst).(*tree.FunctionType)
	fmt.Printf("subst FunctionType: %v\n", funcTy)

	for i, tyArg := range typeArgs {
		tyParam := funcTy.Signature.TypeParams.Params[i]
		c.TyCtx.AddRelation(RelationSatisfies{Type: tyArg, Constraint: tyParam.Constraint})
		c.TyCtx.AddEq(tyArg, subst[tyParam.Name])
	}
	for _, tyParam := range funcTy.Signature.TypeParams.Params {
		ty := &tree.TypeParam{Name: tyParam.Name, Bound: tyParam.Constraint}
		c.TyCtx.AddRelation(RelationSubtype{Sub: ty, Super: tyParam.Constraint})
	}
	for i, arg := range expr.Args {
		var param *tree.ParameterDecl
		if variadicIndex != -1 && i >= variadicIndex {
			param = variadicParam
		} else {
			param = funcTy.Signature.Params.Params[i]
		}

		argTy := c.Synth(arg)

		switch paramTy := param.Type.(type) {
		case *tree.TypeParam:
			c.TyCtx.AddEq(argTy, paramTy)
		default:
			c.CheckAssignableTo(argTy, paramTy)
		}
	}
	returns := []tree.Type{}
	for _, result := range funcTy.Signature.Results.Params {
		returns = append(returns, result.Type)
	}
	switch len(returns) {
	case 0:
		return &tree.TupleType{Elems: []tree.Type{}}
	case 1:
		return returns[0]
	default:
		return &tree.TupleType{Elems: returns}
	}
}

func (c *Checker) SynthFuncLitExpr(expr *tree.FuncLitExpr) tree.Type {
	return &tree.FunctionType{Signature: expr.Signature}
}

func (c *Checker) SynthConversionExpr(expr *tree.ConversionExpr) tree.Type {
	// TODO check conversion
	return expr.Type
}

func (c *Checker) SynthBuiltinConversion(expr tree.Expr, targetTy *tree.TypeBuiltin) tree.Type {
	exprTy := c.Synth(expr)
	switch exprTy := exprTy.(type) {
	case *tree.UntypedConstantType:
		if exprTy.IsCompatible(targetTy.Name.Value) {
			return targetTy
		}
	case *tree.TypeBuiltin:
		if exprTy.Name.Value == "Pointer" {
			if targetTy.Name.Value == "uintptr" {
				return targetTy
			}
		}
	}
	panic(fmt.Sprintf("cannot convert %v to %v", exprTy, targetTy))
}

func (c *Checker) SynthBuiltinFunctionCall(f *tree.BuiltinFunctionType, expr *tree.CallExpr) tree.Type {
	switch f.Name {
	case "new":
		return c.SynthBuiltinNewCall(expr)
	case "make":
		return c.SynthBuiltinMakeCall(expr)
	case "append":
		return c.SynthBuiltinAppendCall(expr)
	case "len":
		return c.SynthBuiltinLenCall(expr)
	case "panic":
		return c.SynthBuiltinPanicCall(expr)
	case "print":
		return c.SynthBuiltinPrintCall(expr)
	case "println":
		return c.SynthBuiltinPrintlnCall(expr)
	case "copy":
		return c.SynthBuiltinCopyCall(expr)
	default:
		spew.Dump(f)
		panic("unreachable")
	}
}

func (c *Checker) SynthBuiltinNewCall(expr *tree.CallExpr) tree.Type {
	if len(expr.Args) != 1 {
		panic("builtin new() takes exactly one argument")
	}
	argTy, ok := c.Synth(expr.Args[0]).(*tree.TypeOfType)
	if !ok {
		panic("new() with non-type argument")
	}
	return &tree.PointerType{BaseType: argTy.Type}
}

func (c *Checker) SynthBuiltinMakeCall(expr *tree.CallExpr) tree.Type {
	if len(expr.Args) == 0 {
		panic("builtin make() takes at least one argument")
	}
	argTy, ok := c.Synth(expr.Args[0]).(*tree.TypeOfType)
	if !ok {
		panic("make() with non-type argument")
	}
	elemTy := argTy.Type
	switch elemTy.(type) {
	case *tree.SliceType:
	case *tree.MapType:
	case *tree.ChannelType:
	default:
		panic("make() with non-slice, non-map, non-channel type")
	}
	for _, arg := range expr.Args[1:] {
		c.CheckExpr(arg, c.BuiltinType("int"))
	}
	return elemTy
}

func (c *Checker) SynthBuiltinAppendCall(expr *tree.CallExpr) tree.Type {
	if len(expr.Args) < 2 {
		panic("builtin append() takes at least two arguments")
	}
	firstTy := c.Synth(expr.Args[0])
	sliceTy, ok := firstTy.(*tree.SliceType)
	if !ok {
		panic("append() with non-slice type")
	}
	for _, arg := range expr.Args[1:] {
		c.CheckAssignableTo(c.Synth(arg), sliceTy.ElemType)
	}
	return sliceTy
}

func (c *Checker) SynthBuiltinLenCall(expr *tree.CallExpr) tree.Type {
	if len(expr.Args) != 1 {
		panic("builtin len() takes exactly one argument")
	}

	argTy := c.Synth(expr.Args[0])

	if !c.HasLen(argTy) {
		panic(fmt.Sprintf("len() on incompatible type %v", argTy))
	}

	return c.BuiltinType("int")
}

func (c *Checker) IsLike(ty tree.Type, pred func(ty tree.Type) bool) bool {
	switch argTy := c.Under(ty).(type) {
	case *tree.TypeParam:
		tyset := c.InterfaceTypeSet(argTy.Bound)
		if tyset.Universe {
			return false
		}
		for _, t := range tyset.Types {
			if !pred(t) {
				return false
			}
		}
		return true
	default:
		return pred(argTy)
	}
}

func (c *Checker) IsByteArray(ty tree.Type) bool {
	return c.IsLike(ty, func(ty tree.Type) bool {
		switch ty := ty.(type) {
		case *tree.SliceType:
			return c.Identical(c.ResolveType(ty.ElemType), c.BuiltinType("byte"))
		default:
			return false
		}
	})
}

func (c *Checker) IsStringLike(ty tree.Type) bool {
	return c.IsLike(ty, func(ty tree.Type) bool {
		switch ty := ty.(type) {
		case *tree.TypeBuiltin:
			return ty.Name.Value == "string"
		case *tree.UntypedConstantType:
			return ty.IsCompatible("string")
		default:
			return false
		}
	})
}

func (c *Checker) HasLen(ty tree.Type) bool {
	return c.IsLike(ty, func(ty tree.Type) bool {
		switch ty := ty.(type) {
		case *tree.SliceType:
		case *tree.ArrayType:
		case *tree.MapType:
		case *tree.ChannelType:
		case *tree.TypeBuiltin:
			return ty.Name.Value == "string"
		case *tree.UntypedConstantType:
			return ty.IsCompatible("string")
		default:
			return false
		}
		return true
	})
}

func (c *Checker) SynthBuiltinPanicCall(expr *tree.CallExpr) tree.Type {
	if len(expr.Args) != 1 {
		panic("builtin panic() takes exactly one argument")
	}
	c.CheckExpr(expr.Args[0], tree.EmptyInterface())
	return &tree.BottomType{}
}

func (c *Checker) SynthBuiltinPrintCall(expr *tree.CallExpr) tree.Type {
	for _, arg := range expr.Args {
		c.Synth(arg)
	}
	return &tree.VoidType{}
}

func (c *Checker) SynthBuiltinPrintlnCall(expr *tree.CallExpr) tree.Type {
	for _, arg := range expr.Args {
		c.Synth(arg)
	}
	return &tree.VoidType{}
}

func (c *Checker) SynthBuiltinCopyCall(expr *tree.CallExpr) tree.Type {
	if len(expr.Args) != 2 {
		panic("builtin copy() takes exactly two arguments")
	}

	ty1 := c.Synth(expr.Args[0])
	ty2 := c.Synth(expr.Args[1])

	if c.IsByteArray(ty1) && c.IsStringLike(ty2) {
		return c.BuiltinType("int")
	}

	var elemTy1, elemTy2 tree.Type

	switch ty1 := c.Under(ty1).(type) {
	case *tree.SliceType:
		elemTy1 = ty1.ElemType
	default:
		panic(fmt.Sprintf("copy() with non-slice type %v", ty1))
	}

	switch ty2 := c.Under(ty2).(type) {
	case *tree.SliceType:
		elemTy2 = ty2.ElemType
	default:
		panic(fmt.Sprintf("copy() with non-slice type %v", ty2))
	}

	c.TyCtx.AddEq(elemTy1, elemTy2)

	return c.BuiltinType("int")
}

// ========================

func (c *Checker) SynthLiteralExpr(expr *tree.LiteralExpr) tree.Type {
	// TODO untype literal types
	switch expr.Literal.(type) {
	case *tree.LiteralInt:
		return &tree.UntypedConstantType{Kind: tree.UntypedConstantInt}
	case *tree.LiteralBool:
		return &tree.UntypedConstantType{Kind: tree.UntypedConstantBool}
	case *tree.LiteralString:
		return &tree.UntypedConstantType{Kind: tree.UntypedConstantString}
	case *tree.LiteralFloat:
		return &tree.UntypedConstantType{Kind: tree.UntypedConstantFloat}
	case *tree.LiteralRune:
		return &tree.UntypedConstantType{Kind: tree.UntypedConstantRune}
	default:
		panic("unreachable")
	}
}

func (c *Checker) UntypedDefaultType(ty *tree.UntypedConstantType) tree.Type {
	switch ty.Kind {
	case tree.UntypedConstantInt:
		return c.BuiltinType("int")
	case tree.UntypedConstantBool:
		return c.BuiltinType("bool")
	case tree.UntypedConstantString:
		return c.BuiltinType("string")
	case tree.UntypedConstantFloat:
		return c.BuiltinType("float64")
	case tree.UntypedConstantRune:
		return c.BuiltinType("rune")
	default:
		panic("unreachable")
	}
}

func (c *Checker) SynthCompositeLitExpr(expr *tree.CompositeLitExpr) tree.Type {
	return c.MakeCompositeLit(expr, c.ResolveType(expr.Type))
}

func (c *Checker) MakeCompositeLit(expr *tree.CompositeLitExpr, targetTy tree.Type) tree.Type {
	underTy := c.Under(targetTy)
	switch underTy := underTy.(type) {
	case *tree.PointerType:
		elemTy := c.ResolveType(underTy.BaseType)
		if _, ok := c.Under(elemTy).(*tree.PointerType); ok {
			panic("composite literal of double pointer type?")
		}
		return &tree.PointerType{BaseType: c.MakeCompositeLit(expr, elemTy)}
	}

	switch exprTy := c.Under(targetTy).(type) {
	case *tree.StructType:
		return c.MakeCompositeLitStruct(expr, exprTy)
	case *tree.SliceType:
		return c.MakeCompositeLitSlice(expr, exprTy)
	case *tree.MapType:
		return c.MakeCompositeLitMap(expr, exprTy)
	case *tree.ArrayType:
		return c.MakeCompositeLitArray(expr, exprTy)
	default:
		spew.Dump(exprTy)
		panic("unreachable")
	}
}

func (c *Checker) MakeCompositeLitStruct(expr *tree.CompositeLitExpr, structTy *tree.StructType) tree.Type {
	if len(expr.Elems) == 0 {
		return expr.Type
	}

	ordered := expr.Elems[0].Key == nil

	if ordered {
		if len(expr.Elems) != len(structTy.Fields) {
			panic("composite literal with wrong number of fields")
		}
		for i, elem := range expr.Elems {
			if elem.Key != nil {
				panic("composite literal with ordered fields")
			}
			field := structTy.Fields[i]
			c.CheckExpr(elem.Value, field.Type)
		}
	} else {
	elems:
		for _, elem := range expr.Elems {
			for _, field := range structTy.Fields {
				if elem.Key == nil {
					panic("composite literal with unordered fields")
				}

				var targetFieldName Identifier
				switch keyTy := elem.Key.(type) {
				case *tree.NameExpr:
					targetFieldName = keyTy.Name
				case *tree.TypeExpr:
					// TODO quirk of the name resolver
					switch keyTy := keyTy.Type.(type) {
					case *tree.PackageTypeName:
						targetFieldName = keyTy.Name
					default:
						spew.Dump(keyTy)
						panic("composite literal must use identifier as key name")
					}
				case *tree.ImportNameExpr:
					// TODO quirk of the name resolver
					targetFieldName = keyTy.Name
				case *tree.PackageNameExpr:
					// TODO quirk of the name resolver
					targetFieldName = keyTy.Name
				default:
					spew.Dump(keyTy)
					panic("composite literal must use identifier as key name")
				}

				if field.Name == targetFieldName {
					c.CheckExpr(elem.Value, field.Type)
					continue elems
				}
			}
			panic(fmt.Sprintf("type %v has no field %v", structTy, elem.Key))
		}
	}

	return expr.Type
}

func (c *Checker) MakeCompositeLitSlice(expr *tree.CompositeLitExpr, sliceTy *tree.SliceType) tree.Type {
	for _, elem := range expr.Elems {
		c.CheckExpr(elem.Value, c.ResolveType(sliceTy.ElemType))
	}
	return expr.Type
}

func (c *Checker) MakeCompositeLitMap(expr *tree.CompositeLitExpr, mapTy *tree.MapType) tree.Type {
	for _, elem := range expr.Elems {
		c.CheckExpr(elem.Key, c.ResolveType(mapTy.KeyType))
		c.CheckExpr(elem.Value, c.ResolveType(mapTy.ElemType))
	}
	return expr.Type
}

func (c *Checker) MakeCompositeLitArray(expr *tree.CompositeLitExpr, arrayTy *tree.ArrayType) tree.Type {
	var arrayLen int
	switch arrayTy.Len.(type) {
	case *tree.EllipsisExpr:
		arrayLen = -1
	default:
		arrayLen = c.EvaluateConstantIntExpr(arrayTy.Len)
		if len(expr.Elems) > arrayLen {
			panic("composite literal with wrong number of elements")
		}
	}
	for _, elem := range expr.Elems {
		c.CheckExpr(elem.Value, arrayTy.ElemType)
	}
	if arrayLen == -1 {
		return &tree.ArrayType{ElemType: arrayTy.ElemType, Len: &tree.ConstIntExpr{Value: len(expr.Elems)}}
	} else {
		return &tree.ArrayType{ElemType: arrayTy.ElemType, Len: &tree.ConstIntExpr{Value: arrayLen}}
	}
}

func (c *Checker) EvaluateConstantIntExpr(expr tree.Expr) int {
	switch expr := expr.(type) {
	case *tree.NameExpr:
		return c.EvaluateConstantIntExpr(c.LookupConst(expr.Name))
	case *tree.PackageNameExpr:
		return c.EvaluateConstantIntExpr(c.PackageLookupConst(expr.Path, expr.Name))
	case *tree.ConstIntExpr:
		return expr.Value
	case *tree.LiteralExpr:
		switch lit := expr.Literal.(type) {
		case *tree.LiteralInt:
			value, err := strconv.Atoi(lit.Value)
			if err != nil {
				panic(fmt.Errorf("invalid integer literal %v", lit.Value))
			}
			return value
		default:
			panic("non-integer literal")
		}
	case *tree.BinaryExpr:
		left := c.EvaluateConstantIntExpr(expr.Left)
		right := c.EvaluateConstantIntExpr(expr.Right)
		switch expr.Op {
		case tree.BinaryOpAdd:
			return left + right
		case tree.BinaryOpSub:
			return left - right
		case tree.BinaryOpMul:
			return left * right
		case tree.BinaryOpQuo:
			return left / right
		case tree.BinaryOpRem:
			return left % right
		case tree.BinaryOpAnd:
			return left & right
		case tree.BinaryOpOr:
			return left | right
		case tree.BinaryOpXor:
			return left ^ right
		case tree.BinaryOpAndNot:
			return left &^ right
		case tree.BinaryOpShl:
			return left << uint(right)
		case tree.BinaryOpShr:
			return left >> uint(right)
		default:
			panic("non-integer binary operator")
		}
	case *tree.UnaryExpr:
		value := c.EvaluateConstantIntExpr(expr.Expr)
		switch expr.Op {
		case tree.UnaryOpPos:
			return value
		case tree.UnaryOpNeg:
			return -value
		case tree.UnaryOpBitNot:
			return ^value
		default:
			panic("non-integer unary operator")
		}
	default:
		spew.Dump(expr)
		panic("non-integer expression")
	}
}

func (c *Checker) TypeApplication(app *tree.TypeApplication) tree.Type {
	return c.TypeApplicationFunc(app, func(*tree.TypeParamDecl, tree.Type) {})
}

func (c *Checker) TypeApplicationFunc(app *tree.TypeApplication, argF func(tyParam *tree.TypeParamDecl, tyArg tree.Type)) tree.Type {
	named, subst := c.InstantiateTypeFunc(app, argF)
	gen := c.ResolveType(named.Type).(*tree.GenericType)
	return c.ApplySubst(gen.Type, subst)
}

func (c *Checker) InstantiateType(app *tree.TypeApplication) (*tree.NamedType, Subst) {
	return c.InstantiateTypeFunc(app, func(*tree.TypeParamDecl, tree.Type) {})
}

func (c *Checker) InstantiateTypeFunc(app *tree.TypeApplication, argF func(tyParam *tree.TypeParamDecl, tyArg tree.Type)) (*tree.NamedType, Subst) {
	named, ok := c.ResolveType(app.Type).(*tree.NamedType)
	if !ok {
		panic("can only instantiate named types?")
	}
	gen, ok := c.ResolveType(named.Type).(*tree.GenericType)
	if !ok {
		panic("not a generic type")
	}
	if len(gen.TypeParams.Params) != len(app.Args) {
		panic("wrong number of type arguments")
	}
	subst := Subst{}
	for i, tyArg := range app.Args {
		tyParam := gen.TypeParams.Params[i]
		subst[tyParam.Name] = tyArg
		c.TyCtx.AddRelation(RelationSatisfies{Type: tyArg, Constraint: tyParam.Constraint})
		argF(tyParam, tyArg)
	}
	return named, subst
}

// ========================

type Subst map[Identifier]tree.Type

func (s Subst) String() string {
	parts := make([]string, 0, len(s))
	for k, v := range s {
		parts = append(parts, fmt.Sprintf("%v -> %v", k, v))
	}
	return fmt.Sprintf("{{ %v }}", strings.Join(parts, " ; "))
}

func (c *Checker) ApplySubst(ty tree.Type, subst Subst) tree.Type {
	switch ty := ty.(type) {
	case *tree.TypeParam:
		if substTy, ok := subst[ty.Name]; ok {
			return substTy
		}
		return ty
	case *tree.TypeName:
		// TODO kinda sus
		if substTy, ok := subst[ty.Name]; ok {
			return substTy
		}
		return ty
	case *tree.PackageTypeName:
		return ty
	case *tree.TypeBuiltin:
		return ty
	case *tree.TypeApplication:
		args := make([]tree.Type, len(ty.Args))
		for i, arg := range ty.Args {
			args[i] = c.ApplySubst(arg, subst)
		}
		return &tree.TypeApplication{Type: ty.Type, Args: args}
	case *tree.ArrayType:
		return &tree.ArrayType{
			ElemType: c.ApplySubst(ty.ElemType, subst),
			Len:      ty.Len,
		}
	case *tree.FunctionType:
		return &tree.FunctionType{
			Signature: c.ApplySubstSignature(ty.Signature, subst),
		}
	case *tree.GenericType:
		return &tree.GenericType{
			TypeParams: c.ApplySubstTypeParamList(ty.TypeParams, subst),
			Type:       c.ApplySubst(ty.Type, subst),
		}
	case *tree.StructType:
		fields := make([]*tree.FieldDecl, len(ty.Fields))
		for i, field := range ty.Fields {
			fields[i] = &tree.FieldDecl{
				Name: field.Name,
				Type: c.ApplySubst(field.Type, subst),
			}
		}
		return &tree.StructType{Fields: fields}
	case *tree.PointerType:
		return &tree.PointerType{BaseType: c.ApplySubst(ty.BaseType, subst)}
	case *tree.InterfaceType:
		constraints := make([]*tree.TypeConstraint, len(ty.Constraints))
		for i, constraint := range ty.Constraints {
			constraints[i] = &tree.TypeConstraint{TypeElem: c.ApplySubstTypeElem(constraint.TypeElem, subst)}
		}
		return &tree.InterfaceType{
			Methods:     c.ApplySubstMethodList(ty.Methods, subst),
			Constraints: constraints,
		}
	case *tree.SliceType:
		return &tree.SliceType{ElemType: c.ApplySubst(ty.ElemType, subst)}
	case *tree.MapType:
		return &tree.MapType{
			KeyType:  c.ApplySubst(ty.KeyType, subst),
			ElemType: c.ApplySubst(ty.ElemType, subst),
		}
	case *tree.ChannelType:
		return &tree.ChannelType{
			ElemType: c.ApplySubst(ty.ElemType, subst),
			Dir:      ty.Dir,
		}
	case *tree.TupleType:
		elems := make([]tree.Type, len(ty.Elems))
		for i, elem := range ty.Elems {
			elems[i] = c.ApplySubst(elem, subst)
		}
		return &tree.TupleType{Elems: elems}
	case *tree.NamedType:
		return &tree.NamedType{
			Name: ty.Name,
			Type: c.ApplySubst(ty.Type, subst),
			// Methods: c.ApplySubstMethodList(ty.Methods, subst),
		}
	case *tree.NilType:
		return ty
	case *tree.UntypedConstantType:
		return ty
	case *tree.TypeOfType:
		return &tree.TypeOfType{Type: c.ApplySubst(ty.Type, subst)}
	case *tree.QualIdentifier:
		return ty
	default:
		spew.Dump(ty)
		panic("unreachable")
	}
}

func (c *Checker) ApplySubstMethodList(methods []*tree.MethodElem, subst Subst) []*tree.MethodElem {
	out := make([]*tree.MethodElem, len(methods))
	for i, method := range methods {
		out[i] = &tree.MethodElem{
			Name: method.Name,
			Type: c.ApplySubst(method.Type, subst).(*tree.FunctionType),
		}
	}
	return out
}

func (c *Checker) ApplySubstSignature(sig *tree.Signature, subst Subst) *tree.Signature {
	return &tree.Signature{
		TypeParams: c.ApplySubstTypeParamList(sig.TypeParams, subst),
		Params:     c.ApplySubstParameterList(sig.Params, subst),
		Results:    c.ApplySubstParameterList(sig.Results, subst),
	}
}

func (c *Checker) ApplySubstTypeParamList(list *tree.TypeParamList, subst Subst) *tree.TypeParamList {
	params := make([]*tree.TypeParamDecl, len(list.Params))
	for i, param := range list.Params {
		var name Identifier
		if substTy, ok := subst[param.Name]; ok {
			name = substTy.(*tree.TypeParam).Name
		} else {
			name = param.Name
		}
		params[i] = &tree.TypeParamDecl{
			Name:       name,
			Constraint: c.ApplySubst(param.Constraint, subst).(*tree.InterfaceType),
		}
	}
	return &tree.TypeParamList{Params: params}
}

func (c *Checker) ApplySubstTypeElem(elem *tree.TypeElem, subst Subst) *tree.TypeElem {
	union := make([]*tree.TypeTerm, len(elem.Union))
	for i, term := range elem.Union {
		union[i] = &tree.TypeTerm{Type: c.ApplySubst(term.Type, subst), Tilde: term.Tilde}
	}
	return &tree.TypeElem{Union: union}
}

func (c *Checker) ApplySubstParameterList(list *tree.ParameterList, subst Subst) *tree.ParameterList {
	params := make([]*tree.ParameterDecl, len(list.Params))
	for i, param := range list.Params {
		params[i] = &tree.ParameterDecl{
			Name:     param.Name,
			Type:     c.ApplySubst(param.Type, subst),
			Variadic: param.Variadic,
		}
	}
	return &tree.ParameterList{Params: params}
}

// ========================

func (c *Checker) Simplify(subst Subst) Subst {
	next := Subst{}
	for k, v := range subst {
		next[k] = c.ApplySubst(v, subst)
	}
	return next
}

func (c *Checker) Merge(lhs, rhs Subst) Subst {
	result := Subst{}
	for k, v := range lhs {
		result[k] = v
	}
	for k, v := range rhs {
		if _, ok := result[k]; ok {
			if !c.Identical(result[k], v) {
				spew.Dump(k, v)
				panic("incompatible substitutions")
			}
		}
		result[k] = v
	}
	return result
}

func (c *Checker) Verify() Subst {
	subst := Subst{}

	fmt.Println("=== Verify ===")

	for i := 0; i < 10; i++ {
		fmt.Printf("=== iteration %d ===\n", i)
		fmt.Println(c.TyCtx)

		learned := Subst{}

		c.Unify(c.TyCtx.Relations, learned)
		learned = c.Simplify(learned)

		fmt.Printf("learned: %v\n", learned)

		next := []Relation{}

		for _, rel := range c.TyCtx.Relations {
			switch rel := rel.(type) {
			case RelationEq:
				next = append(next, RelationEq{
					Left:  c.ApplySubst(rel.Left, learned),
					Right: c.ApplySubst(rel.Right, learned),
				})
			case RelationSubtype:
				next = append(next, RelationSubtype{
					Sub:   c.ApplySubst(rel.Sub, learned),
					Super: c.ApplySubst(rel.Super, learned),
				})
			case RelationSatisfies:
				next = append(next, RelationSatisfies{
					Type:       c.ApplySubst(rel.Type, learned),
					Constraint: c.ApplySubst(rel.Constraint, learned).(*tree.InterfaceType),
				})
			default:
				panic("unreachable")
			}
		}

		c.TyCtx.Relations = next
		subst = c.Merge(subst, learned)

		if len(learned) == 0 {
			break
		}
	}

	subst = c.Simplify(subst)

	fmt.Println("=== subst ===")
	fmt.Println(subst)

	return subst
}

func (c *Checker) Unify(rels []Relation, subst Subst) {
	for _, rel := range rels {
		switch rel := rel.(type) {
		case RelationEq:
			c.UnifyEq(rel.Left, rel.Right, subst)
		case RelationSubtype:
			c.UnifySubtype(rel.Sub, rel.Super, subst)
		case RelationSatisfies:
			c.UnifySatisfies(rel.Type, rel.Constraint, subst)
		default:
			panic("unreachable")
		}
	}
}

func IntersectInterfaces(elems ...tree.InterfaceType) *tree.InterfaceType {
	inter := &tree.InterfaceType{Methods: nil, Constraints: nil}
	for _, elem := range elems {
		inter.Methods = append(inter.Methods, elem.Methods...)
		inter.Constraints = append(inter.Constraints, elem.Constraints...)
	}
	return inter
}

func (c *Checker) UnifyEq(left, right tree.Type, subst Subst) {
	left = c.ResolveType(left)
	right = c.ResolveType(right)

	fmt.Printf("? %v = %v %v\n", left, right, subst)

	if c.Identical(left, right) {
		return
	}

	if _, ok := right.(*tree.TypeParam); ok {
		left, right = right, left
	}

	if _, ok := left.(*tree.NilType); ok {
		left, right = right, left
	}

	if _, ok := right.(*tree.NilType); ok {
		switch left := c.Under(left).(type) {
		case *tree.PointerType:
			return
		case *tree.ChannelType:
			return
		case *tree.FunctionType:
			return
		case *tree.InterfaceType:
			return
		case *tree.MapType:
			return
		case *tree.SliceType:
			return
		case *tree.TypeBuiltin:
			if left.Name.Value == "Pointer" {
				return
			}
			panic(fmt.Sprintf("cannot assign nil to type %v", left))
		default:
			panic(fmt.Sprintf("cannot assign nil to type %v", left))
		}
	}

	switch left := left.(type) {
	case *tree.TypeBuiltin:
		if right, ok := right.(*tree.TypeBuiltin); ok {
			if c.IsNumeric(left) && c.IsNumeric(right) {
				return // TODO ok?
			}
			panic(fmt.Sprintf("cannot unify: %v = %v", left, right))
		}
		c.UnifyEq(right, left, subst)
	case *tree.TypeParam:
		if right, ok := right.(*tree.UntypedConstantType); ok {
			c.UnifyEq(left, c.UntypedDefaultType(right), subst)
			return
		}
		if s, ok := subst[left.Name]; ok {
			if !c.Identical(s, right) {
				c.UnifyEq(s, right, subst)
			}
		} else {
			subst[left.Name] = right
		}
	case *tree.SliceType:
		if right, ok := right.(*tree.SliceType); ok {
			c.UnifyEq(left.ElemType, right.ElemType, subst)
			return
		}
		panic(fmt.Sprintf("cannot unify: %v = %v", left, right))
	case *tree.ArrayType:
		if right, ok := right.(*tree.ArrayType); ok {
			leftLen := c.EvaluateConstantIntExpr(left.Len)
			rightLen := c.EvaluateConstantIntExpr(right.Len)
			if leftLen != rightLen {
				panic(fmt.Sprintf("cannot unify: %v = %v (different array length)", left, right))
			}
			c.UnifyEq(left.ElemType, right.ElemType, subst)
			return
		}
	case *tree.InterfaceType:
		left = c.SimplifyInterface(left)
		if single, ok := IsSingleTypeUnion(left); ok {
			c.UnifyEq(single, right, subst)
			return
		}
		if _, ok := right.(*tree.NilType); ok {
			return
		}
		spew.Dump(left, right)
		panic("TODO")
	case *tree.TypeApplication:
		if right, ok := right.(*tree.TypeApplication); ok {
			if left.Type != right.Type {
				panic(fmt.Sprintf("cannot unify: %v = %v", left, right))
			}
			if len(left.Args) != len(right.Args) {
				panic(fmt.Sprintf("cannot unify: %v = %v", left, right))
			}
			for i, leftArg := range left.Args {
				c.UnifyEq(leftArg, right.Args[i], subst)
			}
			return
		}
		panic(fmt.Sprintf("cannot unify: %v = %v", left, right))
	case *tree.PointerType:
		if right, ok := right.(*tree.PointerType); ok {
			c.UnifyEq(left.BaseType, right.BaseType, subst)
			return
		}
		panic(fmt.Sprintf("cannot unify: %v = %v", left, right))
	case *tree.NamedType:
		c.UnifyEq(c.Under(left), c.Under(right), subst)
	case *tree.UntypedConstantType:
		if right, ok := right.(*tree.TypeBuiltin); ok {
			if left.IsCompatible(right.Name.Value) {
				return
			}
		}
		c.UnifyEq(c.UntypedDefaultType(left), right, subst)
	default:
		spew.Dump(left, right)
		panic("unreachable")
	}
}

func (c *Checker) UnifySubtype(sub, super tree.Type, subst Subst) {
	sub = c.ResolveType(sub)
	super = c.ResolveType(super)

	fmt.Printf("? %v <: %v %v\n", sub, super, subst)

	if sub, ok := c.ResolveType(sub).(*tree.TypeParam); ok {
		if !c.Identical(sub, super) && c.ContainsTypeParam(super, sub) {
			panic(fmt.Sprintf("circular constraint: %v <: %v", sub, super))
		}
	}
	if super, ok := c.ResolveType(super).(*tree.TypeParam); ok {
		if !c.Identical(sub, super) && c.ContainsTypeParam(sub, super) {
			panic(fmt.Sprintf("circular constraint: %v <: %v", sub, super))
		}
	}

	if _, ok := sub.(*tree.NilType); ok {
		switch super := c.Under(super).(type) {
		case *tree.PointerType:
			return
		case *tree.ChannelType:
			return
		case *tree.FunctionType:
			return
		case *tree.InterfaceType:
			return
		case *tree.MapType:
			return
		case *tree.SliceType:
			return
		case *tree.TypeBuiltin:
			if super.Name.Value == "Pointer" {
				return
			}
			panic(fmt.Sprintf("cannot assign nil to type %v", super))
		default:
			panic(fmt.Sprintf("cannot assign nil to type %v", super))
		}
	}

	if c.IsConcreteType(super) {
		c.UnifyEq(sub, super, subst)
		return
	}

	switch super := super.(type) {
	case *tree.InterfaceType:
		var typeset *TypeSet
		c.BasicSatisfy(sub, super, subst, &typeset)
		if typeset != nil && !typeset.Universe {
			// TODO hacky???
			panic(fmt.Sprintf("cannot assign %v to %v", sub, super))
		}
	case *tree.TypeApplication:
		c.UnifySubtype(sub, c.Under(super), subst) // TODO: adding more constraints?
	case *tree.NamedType:
		if subTy, ok := sub.(*tree.NamedType); ok {
			if subTy.Name == super.Name {
				return
			}
		}
		c.UnifySubtype(sub, c.Under(super), subst)
	case *tree.TypeParam:
		if subTy, ok := c.ResolveType(sub).(*tree.TypeParam); ok {
			if subTy.Name == super.Name {
				return
			}
		}
		if super.Bound != nil {
			c.UnifySubtype(sub, super.Bound, subst)
		}
	default:
		spew.Dump(sub, super)
		panic("unreachable")
	}
}

func (c *Checker) UnifySatisfies(sub tree.Type, inter *tree.InterfaceType, subst Subst) {
	sub = c.ResolveType(sub)

	var typeset *TypeSet
	c.BasicSatisfy(sub, inter, subst, &typeset)

	if typeset != nil && !typeset.Universe {
		for _, term := range typeset.Types {
			termTy := c.ResolveType(term)
			if !c.IsConcreteType(termTy) {
				panic("cannot make union of non-concrete types")
			}
			if c.Identical(sub, termTy) {
				c.UnifyEq(sub, termTy, subst) // necessary?
				return
			}
		}
		panic(fmt.Sprintf("type %v does not satisfy %v", sub, inter))
	}
}

// TODO this seems unprincipled
func (c *Checker) BasicSatisfy(sub tree.Type, inter *tree.InterfaceType, subst Subst, out **TypeSet) {
	inter = c.SimplifyInterface(inter)
	supertypeset := c.InterfaceTypeSet(inter)
	if !supertypeset.Universe && len(supertypeset.Types) == 0 {
		panic("cannot satisfy empty set")
	}
	if len(supertypeset.Types) == 1 {
		single := c.ResolveType(supertypeset.Types[0])
		// TODO hacky
		if c.Identical(sub, single) {
			return
		}
		if c.IsConcreteType(single) {
			c.UnifyEq(sub, single, subst)
		}
		// c.UnifySatisfies(sub, &InterfaceType{Methods: supertypeset.Methods}, subst)
		c.TyCtx.AddRelation(RelationSubtype{Sub: sub, Super: &tree.InterfaceType{Methods: supertypeset.Methods}})
		return // leave for next iteration?
	}
	if len(supertypeset.Methods) > 0 {
		subMethods, pointerReceiver := c.MethodSet(sub)
	super:
		for _, superMethod := range supertypeset.Methods {
			for _, subMethod := range subMethods {
				if subMethod.Name == superMethod.Name {
					if subMethod.PointerReceiver && !pointerReceiver {
						panic(fmt.Sprintf("cannot use pointer-receiver method %v with non pointer", subMethod))
					}
					if c.Identical(subMethod.Type, superMethod.Type) {
						continue super
					}
					panic("incompatible method signature")
				}
			}
			panic(fmt.Sprintf("type %v doesn't have method %v", sub, superMethod))
		}
	}
	if tyPar, ok := c.ResolveType(sub).(*tree.TypeParam); ok {
		var bound tree.Type = tyPar.Bound
		if tyPar.Bound == nil {
			bound = tree.EmptyInterface()
		}
		c.UnifySubtype(bound, inter, subst)
		return
	}
	if sub, ok := c.ResolveType(sub).(*tree.InterfaceType); ok {
		subtypeset := c.InterfaceTypeSet(sub)
		if len(subtypeset.Methods) != 0 {
			panic("TODO")
		}
		for _, term := range subtypeset.Types {
			termTy := c.ResolveType(term)
			found := false
			for _, superTerm := range supertypeset.Types {
				superTermTy := c.ResolveType(superTerm)
				if c.Identical(termTy, superTermTy) {
					found = true
				}
			}
			if !found {
				panic(fmt.Sprintf("interface %v does not satisfy %v", sub, inter))
			}
		}
		return
	}
	if len(supertypeset.Types) == 1 {
		c.UnifySubtype(sub, supertypeset.Types[0], subst)
		return
	}
	*out = &supertypeset
}

func (c *Checker) Under(ty tree.Type) tree.Type {
	switch ty := ty.(type) {
	case *tree.NamedType:
		return c.Under(ty.Type)
	case *tree.TypeApplication:
		// TODO: pre apply?
		return c.TypeApplication(ty) // TODO crazy?
	default:
		return ty
	}
}

func (c *Checker) ContainsTypeParam(ty tree.Type, tyParam *tree.TypeParam) bool {
	switch ty := ty.(type) {
	case *tree.TypeParam:
		return ty.Name == tyParam.Name
	case *tree.TypeApplication:
		for _, arg := range ty.Args {
			if c.ContainsTypeParam(arg, tyParam) {
				return true
			}
		}
		return false
	case *tree.PointerType:
		return c.ContainsTypeParam(ty.BaseType, tyParam)
	case *tree.StructType:
		for _, field := range ty.Fields {
			if c.ContainsTypeParam(field.Type, tyParam) {
				return true
			}
		}
		return false
	case *tree.InterfaceType:
		for _, constraint := range ty.Constraints {
			for _, term := range constraint.TypeElem.Union {
				if c.ContainsTypeParam(term.Type, tyParam) {
					return true
				}
			}
		}
		for _, method := range ty.Methods {
			if c.ContainsTypeParam(method.Type, tyParam) {
				return true
			}
		}
		return false
	case *tree.SliceType:
		return c.ContainsTypeParam(ty.ElemType, tyParam)
	case *tree.TypeBuiltin:
		return false
	case *tree.ArrayType:
		return c.ContainsTypeParam(ty.ElemType, tyParam)
	case *tree.FunctionType:
		// could check signature type params, but no nested type params?
		for _, param := range ty.Signature.Params.Params {
			if c.ContainsTypeParam(param.Type, tyParam) {
				return true
			}
		}
		for _, result := range ty.Signature.Results.Params {
			if c.ContainsTypeParam(result.Type, tyParam) {
				return true
			}
		}
		return false
	case *tree.GenericType:
		// cannot nest generic types?
		return false
	case *tree.TupleType:
		for _, elem := range ty.Elems {
			if c.ContainsTypeParam(elem, tyParam) {
				return true
			}
		}
		return false
	default:
		return false
	}
}

func (c *Checker) MethodSet(ty tree.Type) ([]*tree.MethodElem, bool) {
	ty = c.ResolveType(ty)

	var pointerReceiver bool
	if pointerTy, ok := ty.(*tree.PointerType); ok {
		ty = pointerTy.BaseType
		pointerReceiver = true
	}

	switch ty := c.ResolveType(ty).(type) {
	case *tree.InterfaceType:
		return ty.Methods, false
	case *tree.NamedType:

		return c.NamedTypeMethods(ty), pointerReceiver
	case *tree.TypeApplication:
		named, subst := c.InstantiateType(ty)
		var methods []*tree.MethodElem
		for _, m := range c.NamedTypeMethods(named) {
			methods = append(methods, &tree.MethodElem{
				Name: m.Name,
				Type: c.ApplySubst(m.Type, subst).(*tree.FunctionType),
			})
		}
		return methods, pointerReceiver
	default:
		spew.Dump(ty)
		panic(fmt.Sprintf("type %v cannot have methods", ty))
	}
}

func (c *Checker) NamedTypeMethods(namedTy *tree.NamedType) []*tree.MethodElem {
	if interTy, ok := c.Under(namedTy).(*tree.InterfaceType); ok {
		return interTy.Methods
	}

	var methods []*tree.MethodElem

	c.VarCtx.Iter(func(name Identifier, ty tree.Type) {
		if strings.HasPrefix(name.Value, namedTy.Name.Value+".") {
			methodTy := ty.(*tree.MethodType)
			methods = append(methods, &tree.MethodElem{
				Name:            Identifier{Value: name.Value[len(namedTy.Name.Value)+1:]},
				Type:            methodTy.Type,
				PointerReceiver: methodTy.PointerReceiver,
			})
		}
	})

	return methods
}

func (c *Checker) IsTypeParam(ty tree.Type) bool {
	switch ty.(type) {
	case *tree.TypeParam:
		return true
	default:
		return false
	}
}

func (c *Checker) IsConcreteType(ty tree.Type) bool {
	switch ty := ty.(type) {
	case *tree.NamedType:
		return c.IsConcreteType(c.ResolveType(ty.Type))
	case *tree.TypeOfType:
		return c.IsConcreteType(ty.Type)
	case *tree.TypeBuiltin:
		return true
	case *tree.InterfaceType:
		return false
	case *tree.TypeParam:
		return false
	case *tree.PointerType:
		return true
	case *tree.SliceType:
		return true
	case *tree.ArrayType:
		return true
	case *tree.StructType:
		return true
	case *tree.TypeApplication:
		return c.IsConcreteType(c.TypeApplication(ty))
	case *tree.UntypedConstantType:
		return true
	case *tree.FunctionType:
		return true
	default:
		spew.Dump(ty)
		panic("unreachable")
	}
}

func (c *Checker) Identical(ty1, ty2 tree.Type) bool {
	// fmt.Printf("== Identical(%v, %v) ==\n", ty1, ty2)
	// TODO recursive types?
	switch ty1 := ty1.(type) {
	case *tree.NamedType:
		if ty2, ok := ty2.(*tree.NamedType); ok {
			return ty1.Name == ty2.Name
		}
		return false
	case *tree.TypeBuiltin:
		ty, ok := ty2.(*tree.TypeBuiltin)
		if !ok {
			return false
		}
		return ty1.Name == ty.Name
	case *tree.TypeParam:
		if ty2, ok := ty2.(*tree.TypeParam); ok {
			return ty1.Name == ty2.Name
		}
		return false
	case *tree.InterfaceType:
		if ty2, ok := ty2.(*tree.InterfaceType); ok {
			if len(ty1.Methods) != len(ty2.Methods) {
				return false
			}
			if len(ty1.Constraints) != len(ty2.Constraints) {
				return false
			}
			panic("TODO")
		}
		return false
	case *tree.PointerType:
		if ty2, ok := ty2.(*tree.PointerType); ok {
			return c.Identical(c.ResolveType(ty1.BaseType), c.ResolveType(ty2.BaseType))
		}
		return false
	case *tree.TypeApplication:
		if ty2, ok := ty2.(*tree.TypeApplication); ok {
			if ty1.Type != ty2.Type {
				return false
			}
			if len(ty1.Args) != len(ty2.Args) {
				return false
			}
			for i, arg := range ty1.Args {
				if !c.Identical(c.ResolveType(arg), c.ResolveType(ty2.Args[i])) {
					return false
				}
			}
			return true
		}
		return false
	case *tree.ArrayType:
		if ty2, ok := ty2.(*tree.ArrayType); ok {
			if ty1.Len != ty2.Len {
				return false
			}
			return c.Identical(ty1.ElemType, ty2.ElemType)
		}
		return false
	case *tree.StructType:
		if ty2, ok := ty2.(*tree.StructType); ok {
			if len(ty1.Fields) != len(ty2.Fields) {
				return false
			}
			for i, field := range ty1.Fields {
				if field.Name != ty2.Fields[i].Name {
					return false
				}
				if !c.Identical(field.Type, ty2.Fields[i].Type) {
					return false
				}
			}
			return true
		}
		return false
	case *tree.SliceType:
		if ty2, ok := ty2.(*tree.SliceType); ok {
			return c.Identical(c.ResolveType(ty1.ElemType), c.ResolveType(ty2.ElemType))
		}
		return false
	case *tree.FunctionType:
		if ty2, ok := ty2.(*tree.FunctionType); ok {
			return c.IdenticalFunctionTypes(ty1, ty2)
		}
		return false
	case *tree.UntypedConstantType:
		return false // TODO ???
	case *tree.NilType:
		return false // TODO ???
	default:
		spew.Dump(ty1, ty2)
		panic("unreachable")
	}
}

func (c *Checker) IdenticalFunctionTypes(ty1, ty2 *tree.FunctionType) bool {
	if len(ty1.Signature.TypeParams.Params) != 0 {
		panic("cannot compare type with type parameters")
	}
	if len(ty2.Signature.TypeParams.Params) != 0 {
		panic("cannot compare type with type parameters")
	}
	if len(ty1.Signature.Params.Params) != len(ty2.Signature.Params.Params) {
		return false
	}
	if len(ty1.Signature.Results.Params) != len(ty2.Signature.Results.Params) {
		return false
	}
	for i := range ty1.Signature.Params.Params {
		// TODO variadic
		par1 := ty1.Signature.Params.Params[i]
		par2 := ty2.Signature.Params.Params[i]
		if !c.Identical(c.ResolveType(par1.Type), c.ResolveType(par2.Type)) {
			return false
		}
	}
	for i := range ty1.Signature.Results.Params {
		par1 := ty1.Signature.Results.Params[i]
		par2 := ty2.Signature.Results.Params[i]
		if !c.Identical(c.ResolveType(par1.Type), c.ResolveType(par2.Type)) {
			return false
		}
	}
	return true
}

func (c *Checker) InterfaceTypeSet(ty *tree.InterfaceType) TypeSet {
	typeset := TypeSet{
		Methods:  ty.Methods,
		Universe: true,
	}

	if len(ty.Constraints) == 0 {
		return typeset
	}

	for _, constraint := range ty.Constraints {
		var next TypeSet
		if len(constraint.TypeElem.Union) == 1 {
			term := constraint.TypeElem.Union[0]
			if term.Tilde {
				panic("TODO")
			}
			termTy := c.ResolveType(term.Type)
			switch underTy := c.Under(termTy).(type) {
			case *tree.InterfaceType:
				next = c.InterfaceTypeSet(underTy)
			case *tree.TypeParam:
				if underTy.Bound != nil {
					next = c.InterfaceTypeSet(underTy.Bound)
				} else {
					next = TypeSet{Types: []tree.Type{underTy}, Universe: false}
				}
			default:
				next = TypeSet{Types: []tree.Type{termTy}, Universe: false}
			}
		} else {
			var types []tree.Type
			for _, term := range constraint.TypeElem.Union {
				if term.Tilde {
					panic("TODO")
				}
				termTy := c.ResolveType(term.Type)
				switch ty := c.Under(termTy).(type) {
				case *tree.InterfaceType:
					if len(ty.Methods) == 0 {
						spew.Dump(ty)
						panic("cannot make union of interface with methods")
					}
					spew.Dump(ty)
					panic("what")
				default:
					types = append(types, ty)
				}
			}
			next = TypeSet{Types: types, Universe: false}
		}
		typeset = c.Combine(typeset, next)
	}

	return typeset
}

func (c *Checker) TypeSet(con tree.TypeConstraint) TypeSet {
	if len(con.TypeElem.Union) == 1 {
		term := con.TypeElem.Union[0]
		if term.Tilde {
			panic("TODO")
		}
		termTy := c.ResolveType(term.Type)
		switch ty := c.Under(termTy).(type) {
		case *tree.InterfaceType:
			return c.InterfaceTypeSet(ty)
		case *tree.TypeParam:
			if ty.Bound != nil {
				return c.InterfaceTypeSet(ty.Bound)
			}
			return TypeSet{Types: []tree.Type{ty}, Universe: false}
		default:
			return TypeSet{Types: []tree.Type{ty}, Universe: false}
		}
	}
	var types []tree.Type
	for _, term := range con.TypeElem.Union {
		if term.Tilde {
			panic("TODO")
		}
		termTy := c.ResolveType(term.Type)
		switch ty := c.Under(termTy).(type) {
		case *tree.InterfaceType:
			if len(ty.Methods) == 0 {
				spew.Dump(ty)
				panic("cannot make union of interface with methods")
			}
		default:
			types = append(types, ty)
		}
	}
	return TypeSet{Types: types, Universe: false}
}

func (c *Checker) SimplifyInterface(ty *tree.InterfaceType) *tree.InterfaceType {
	if single, ok := IsSingleTypeUnion(ty); ok {
		switch single := single.(type) {
		case *tree.InterfaceType:
			return c.SimplifyInterface(single)
		case *tree.TypeName:
			singleRef := c.Lookup(single.Name)
			if singleRef, ok := singleRef.(*tree.InterfaceType); ok {
				return c.SimplifyInterface(singleRef)
			}
		}
	}
	return ty
}

func IsSingleTypeUnion(ty *tree.InterfaceType) (tree.Type, bool) {
	if len(ty.Methods) == 0 && len(ty.Constraints) == 1 && len(ty.Constraints[0].TypeElem.Union) == 1 {
		return ty.Constraints[0].TypeElem.Union[0].Type, true
	}
	return nil, false
}
