package check

import (
	"fmt"
	"github.com/davecgh/go-spew/spew"
	"github.com/garciat/gobid/algos"
	"github.com/garciat/gobid/common"
	"github.com/garciat/gobid/source"
	"github.com/garciat/gobid/tree"
	"slices"
	"strings"
)

type NameDependencies = common.Map[common.Identifier, common.Set[common.Identifier]]

type WalkResult struct {
	Dependencies common.Set[common.Identifier]
	NameTypeRefs common.Map[common.Identifier, common.Set[*tree.Type]]
	NameExprRefs common.Map[common.Identifier, common.Set[*tree.Expr]]
}

func (wr WalkResult) WithDeps(dep ...common.Identifier) WalkResult {
	return WalkResult{
		Dependencies: common.NewSet(dep...),
		NameTypeRefs: wr.NameTypeRefs,
		NameExprRefs: wr.NameExprRefs,
	}
}

func EmptyResult() WalkResult {
	return WalkResult{
		Dependencies: common.NewSet[common.Identifier](),
		NameTypeRefs: make(common.Map[common.Identifier, common.Set[*tree.Type]]),
		NameExprRefs: make(common.Map[common.Identifier, common.Set[*tree.Expr]]),
	}
}

func NewTypeRef(name common.Identifier, ty *tree.Type) WalkResult {
	return WalkResult{
		Dependencies: common.NewSet(name),
		NameTypeRefs: common.Map[common.Identifier, common.Set[*tree.Type]]{
			name: common.NewSet(ty),
		},
		NameExprRefs: make(common.Map[common.Identifier, common.Set[*tree.Expr]]),
	}
}

func NewExprRef(name common.Identifier, expr *tree.Expr) WalkResult {
	return WalkResult{
		Dependencies: common.NewSet(name),
		NameTypeRefs: make(common.Map[common.Identifier, common.Set[*tree.Type]]),
		NameExprRefs: common.Map[common.Identifier, common.Set[*tree.Expr]]{
			name: common.NewSet(expr),
		},
	}
}

func CombineWalkResults(results ...WalkResult) WalkResult {
	deps := make(common.Set[common.Identifier])
	typeRefs := make(common.Map[common.Identifier, common.Set[*tree.Type]])
	exprRefs := make(common.Map[common.Identifier, common.Set[*tree.Expr]])

	for _, result := range results {
		deps.Merge(result.Dependencies)
		typeRefs.MergeFunc(result.NameTypeRefs, common.MergeSets2[*tree.Type])
		exprRefs.MergeFunc(result.NameExprRefs, common.MergeSets2[*tree.Expr])
	}

	return WalkResult{
		Dependencies: deps,
		NameTypeRefs: typeRefs,
		NameExprRefs: exprRefs,
	}
}

type NameInfo struct {
	Kind     common.DeclKind
	Name     common.Identifier
	Decl     tree.Decl
	Deps     common.Set[common.Identifier]
	ExprRefs common.Set[*tree.Expr]
	TypeRefs common.Set[*tree.Type]
}

type NameContext struct {
	Names common.Map[common.Identifier, *NameInfo]
}

// ====================

func MakeBuiltinNames() common.Map[common.Identifier, *NameInfo] {
	names := make(common.Map[common.Identifier, *NameInfo])

	builtins := MakeBuiltins()

	for name, ty := range builtins.Types {
		var kind common.DeclKind
		switch ty := ty.(type) {
		case *tree.TypeOfType:
			switch ty.Type.(type) {
			case *tree.NamedType:
				kind = common.DeclKindType
			case *tree.BuiltinType:
				kind = common.DeclKindType
			default:
				kind = common.DeclKindAlias
			}
		case *tree.BuiltinFunctionType:
			kind = common.DeclKindFunc
		default:
			kind = common.DeclKindConst
		}
		names[name] = &NameInfo{
			Kind: kind,
			Name: name,
			Deps: common.Set[common.Identifier]{},
		}
	}

	return names
}

// ====================

type MethodsByName = common.Map[common.Identifier, *tree.MethodElem]
type MethodsByNameByReceiver = common.Map[common.Identifier, MethodsByName]

type NameResolutionResult struct {
	SortedDecls []tree.Decl
	Methods     MethodsByNameByReceiver
}

func ResolvePackageNames(pkg *source.Package) NameResolutionResult {
	CheckerPrintf("=== ResolvePackageNames(%v) ===\n", pkg.ImportPath)

	packageNames := make(common.Map[common.Identifier, *NameInfo])
	compositeLitKeys := make(common.Set[common.Identifier])

	packageTypeRefs := make(common.Map[common.Identifier, common.Set[*tree.Type]])
	packageExprRefs := make(common.Map[common.Identifier, common.Set[*tree.Expr]])

	packageMethods := make(MethodsByNameByReceiver)

	for _, file := range pkg.Files {
		walker := NewGraphWalker(packageMethods)

		fileTypeRefs := make(common.Map[common.Identifier, common.Set[*tree.Type]])
		fileExprRefs := make(common.Map[common.Identifier, common.Set[*tree.Expr]])

		for _, decl := range file.Decls {
			res := walker.GraphWalkDecl(decl)
			fileTypeRefs.MergeFunc(res.NameTypeRefs, common.MergeSets2[*tree.Type])
			fileExprRefs.MergeFunc(res.NameExprRefs, common.MergeSets2[*tree.Expr])
		}

		importReplacements := make(common.Map[common.Identifier, tree.Node])
		for id, path := range walker.Imports {
			importReplacements[id] = &tree.ImportRef{ImportPath: path, ImportDeclaredName: id}

			for _, name := range walker.Context.Names {
				name.Deps.Remove(id)
			}
		}
		ApplyReplacements(importReplacements, fileTypeRefs, fileExprRefs)

		err := packageNames.MergeStrict(walker.Context.Names)
		if err != nil {
			panic(fmt.Errorf("duplicate name in file: %v", err))
		}

		packageTypeRefs.MergeFunc(fileTypeRefs, common.MergeSets2[*tree.Type])
		packageExprRefs.MergeFunc(fileExprRefs, common.MergeSets2[*tree.Expr])

		compositeLitKeys.Merge(walker.CompositeLitKeys)
	}

	missingKeysReplacements := make(common.Map[common.Identifier, tree.Node])
	for key := range compositeLitKeys {
		if !packageNames.Contains(key) {
			// drop this for now; wait for later failure, but keep this graph clean
			missingKeysReplacements.Add(key, &tree.NameExpr{Name: key})

			for _, name := range packageNames {
				name.Deps.Remove(key)
			}
		}
	}
	ApplyReplacements(missingKeysReplacements, packageTypeRefs, packageExprRefs)

	packageNameReplacements := make(common.Map[common.Identifier, tree.Node])
	for id, name := range packageNames {
		packageNameReplacements[id] = MakeReplacement(pkg, name)
	}
	ApplyReplacements(packageNameReplacements, packageTypeRefs, packageExprRefs)

	builtinNames := MakeBuiltinNames()
	for id := range builtinNames {
		delete(packageTypeRefs, id)
		delete(packageExprRefs, id)

		for _, name := range packageNames {
			name.Deps.Remove(id)
		}
	}

	if len(packageExprRefs) != 0 || len(packageTypeRefs) != 0 {
		panic(fmt.Errorf("unresolved references:\nexpr=%v\ntype=%v", packageExprRefs, packageTypeRefs))
	}

	dependencies := make(NameDependencies)
	for _, name := range packageNames {
		dependencies[name.Name] = name.Deps

		if name.Name.Value == "waitReasonZero" {
			fmt.Println("waitReasonZero deps:", dependencies[name.Name])
		}
		if name.Name.Value == "waitReason" {
			fmt.Println("waitReason deps:", dependencies[name.Name])
		}
	}
	VerifyNameCycles(packageNames, dependencies)

	sortedNames := TopologicalSort2(packageNames, func(name *NameInfo) common.Set[common.Identifier] {
		return dependencies[name.Name]
	})

	sortedDecls := make([]tree.Decl, 0, len(packageNames))
	for i, name := range sortedNames {
		if name.Kind == common.DeclKindImport {
			// handled outside
			continue
		}
		sortedDecls = append(sortedDecls, name.Decl)
		_ = i
	}

	return NameResolutionResult{
		SortedDecls: sortedDecls,
		Methods:     packageMethods,
	}
}

// ====================

type GraphWalker struct {
	Context          *NameContext
	Imports          common.Map[common.Identifier, common.ImportPath]
	CompositeLitKeys common.Set[common.Identifier]
	Methods          MethodsByNameByReceiver
}

func NewGraphWalker(methods MethodsByNameByReceiver) *GraphWalker {
	return &GraphWalker{
		Context:          &NameContext{Names: make(common.Map[common.Identifier, *NameInfo])},
		Imports:          make(common.Map[common.Identifier, common.ImportPath]),
		CompositeLitKeys: make(common.Set[common.Identifier]),
		Methods:          methods,
	}
}

func (gw *GraphWalker) Define(
	kind common.DeclKind,
	name common.Identifier,
	decl tree.Decl,
	deps common.Set[common.Identifier],
) {
	if name == common.IgnoreIdent {
		name = common.NewIdentifier(fmt.Sprintf("@%v", len(gw.Context.Names)))
	}
	if kind == common.DeclKindFunc && name.Value == "init" {
		name = common.NewIdentifier(fmt.Sprintf("init_%v", len(gw.Context.Names)))
	}
	gw.Context.Names[name] = &NameInfo{
		Kind: kind,
		Name: name,
		Deps: deps,
		Decl: decl,
	}
}

func (gw *GraphWalker) GraphWalkDecl(decl tree.Decl) WalkResult {
	switch decl := decl.(type) {
	case *tree.ImportDecl:
		return gw.GraphWalkImportDecl(decl)
	}

	switch decl := decl.(type) {
	case *tree.ConstDecl:
		return gw.GraphWalkConstDecl(decl)
	case *tree.VarDecl:
		return gw.GraphWalkVarDecl(decl)
	case *tree.TypeDecl:
		return gw.GraphWalkTypeDecl(decl)
	case *tree.AliasDecl:
		return gw.GraphWalkAliasDecl(decl)
	case *tree.FunctionDecl:
		return gw.GraphWalkFunctionDecl(decl)
	case *tree.MethodDecl:
		return gw.GraphWalkMethodDecl(decl)
	default:
		panic(fmt.Errorf("unknown decl type: %T", decl))
	}
}

func (gw *GraphWalker) GraphWalkImportDecl(decl *tree.ImportDecl) WalkResult {
	switch {
	case decl.Alias != nil && *decl.Alias == common.IgnoreIdent:
		return EmptyResult()
	case decl.Alias != nil && *decl.Alias == common.NewIdentifier("."):
		return EmptyResult()
	default:
		gw.Imports[decl.EffectiveName()] = decl.ImportPath
		//gw.Define(common.DeclKindImport, decl.EffectiveName(), decl, common.NewSet[common.Identifier]())
		return EmptyResult()
	}
}

func (gw *GraphWalker) GraphWalkConstDecl(decl *tree.ConstDecl) WalkResult {
	res := CombineWalkResults(
		gw.GraphWalkType(&decl.Type),
		gw.GraphWalkExpr(&decl.Value),
	)

	gw.Define(common.DeclKindConst, decl.Name, decl, res.Dependencies)

	return res.WithDeps(decl.Name)
}

func (gw *GraphWalker) GraphWalkVarDecl(decl *tree.VarDecl) WalkResult {
	res := CombineWalkResults(
		gw.GraphWalkType(&decl.Type),
		gw.GraphWalkExpr(&decl.Expr),
	)

	names := make([]common.Identifier, 0, len(decl.Names))

	for _, name := range decl.Names {
		gw.Define(common.DeclKindVar, name, decl, res.Dependencies)
		names = append(names, name)
	}

	return res.WithDeps(names...)
}

func (gw *GraphWalker) GraphWalkTypeDecl(decl *tree.TypeDecl) WalkResult {
	res := CombineWalkResults(
		gw.GraphWalkTypeParamList(decl.TypeParams),
		gw.GraphWalkType(&decl.Type),
	)

	res.Dependencies.Remove(decl.Name)
	for _, tyParam := range decl.TypeParams.Params {
		res.Dependencies.Remove(tyParam.Name)
		res.NameTypeRefs.Remove(tyParam.Name)
		res.NameExprRefs.Remove(tyParam.Name) // shouldn't have any
	}

	gw.Define(common.DeclKindType, decl.Name, decl, res.Dependencies)

	return res.WithDeps(decl.Name)
}

func (gw *GraphWalker) GraphWalkAliasDecl(decl *tree.AliasDecl) WalkResult {
	res := gw.GraphWalkType(&decl.Type)

	gw.Define(common.DeclKindAlias, decl.Name, decl, res.Dependencies)

	return res.WithDeps(decl.Name)
}

func (gw *GraphWalker) GraphWalkFunctionDecl(decl *tree.FunctionDecl) WalkResult {
	res := gw.GraphWalkSignature(decl.Signature)

	gw.Define(common.DeclKindFunc, decl.Name, decl, res.Dependencies)

	return res.WithDeps(decl.Name)
}

func (gw *GraphWalker) GraphWalkMethodDecl(decl *tree.MethodDecl) WalkResult {
	pointerReceiver := false
	receiverTy := decl.Receiver.Type
	if pointerTy, ok := receiverTy.(*tree.PointerType); ok {
		receiverTy = pointerTy.ElemType
		pointerReceiver = true
	}

	var methodHolder common.Identifier
	var typeParams = common.NewSet[common.Identifier]()

	switch receiverTy := receiverTy.(type) {
	case *tree.TypeName:
		methodHolder = receiverTy.Name
	case *tree.TypeApplication:
		for _, arg := range receiverTy.Args {
			switch arg := arg.(type) {
			case *tree.TypeName:
				typeParams.Add(arg.Name)
			default:
				panic("unexpected")
			}
		}
		switch receiverTy := receiverTy.Type.(type) {
		case *tree.TypeName:
			methodHolder = receiverTy.Name
		case *tree.ImportTypeName:
			panic("cannot define method on imported type")
		default:
			panic("unreachable?")
		}
	default:
		spew.Dump(receiverTy)
		panic("TODO")
	}

	if !gw.Methods.Contains(methodHolder) {
		gw.Methods[methodHolder] = make(MethodsByName)
	}

	receiverMethods := gw.Methods[methodHolder]

	if receiverMethods.Contains(decl.Name) {
		panic(fmt.Errorf("duplicate method: %v", decl.Name))
	}

	receiverMethods[decl.Name] = &tree.MethodElem{
		Name:            decl.Name,
		PointerReceiver: pointerReceiver,
		Type:            &tree.FunctionType{Signature: decl.Signature},
	}

	res := gw.GraphWalkSignature(decl.Signature)

	for tyParam := range typeParams {
		res.Dependencies.Remove(tyParam)
		res.NameTypeRefs.Remove(tyParam)
		res.NameExprRefs.Remove(tyParam) // shouldn't have any
	}

	return res.WithDeps(common.NewIdentifier(fmt.Sprintf("%v.%v", methodHolder, decl.Name)))
}

func (gw *GraphWalker) GraphWalkSignature(sig *tree.Signature) WalkResult {
	res := CombineWalkResults(
		gw.GraphWalkTypeParamList(sig.TypeParams),
		gw.GraphWalkParamList(sig.Params),
		gw.GraphWalkParamList(sig.Results),
	)

	for _, tyParam := range sig.TypeParams.Params {
		res.Dependencies.Remove(tyParam.Name)
		res.NameTypeRefs.Remove(tyParam.Name)
		res.NameExprRefs.Remove(tyParam.Name) // shouldn't have any
	}

	return res
}

func (gw *GraphWalker) GraphWalkTypeParamList(list *tree.TypeParamList) WalkResult {
	allDeps := make([]WalkResult, len(list.Params))
	for i, param := range list.Params {
		allDeps[i] = gw.GraphWalkInterfaceType(param.Constraint)
	}
	return CombineWalkResults(allDeps...)
}

func (gw *GraphWalker) GraphWalkParamList(list *tree.ParameterList) WalkResult {
	allDeps := make([]WalkResult, len(list.Params))
	for i, param := range list.Params {
		allDeps[i] = gw.GraphWalkType(&param.Type)
	}
	return CombineWalkResults(allDeps...)
}

func (gw *GraphWalker) GraphWalkType(arg *tree.Type) WalkResult {
	if *arg == nil {
		return EmptyResult()
	}
	switch ty := (*arg).(type) {
	case *tree.NamedType:
		panic("doesn't exist yet")
	case *tree.TypeName:
		return NewTypeRef(ty.Name, arg)
	case *tree.ImportTypeName:
		return gw.GraphWalkImportTypeName(ty)
	case *tree.TypeApplication:
		return gw.GraphWalkTypeApplication(ty)
	case *tree.InterfaceType:
		return gw.GraphWalkInterfaceType(ty)
	case *tree.StructType:
		return gw.GraphWalkStructType(ty)
	case *tree.ArrayType:
		return gw.GraphWalkArrayType(ty)
	case *tree.FunctionType:
		return gw.GraphWalkSignature(ty.Signature)
	case *tree.SliceType:
		return gw.GraphWalkType(&ty.ElemType)
	case *tree.MapType:
		return gw.GraphWalkMapType(ty)
	case *tree.PointerType:
		return gw.GraphWalkType(&ty.ElemType)
	case *tree.ChannelType:
		return gw.GraphWalkType(&ty.ElemType)
	default:
		spew.Dump(ty)
		panic("unreachable")
	}
}

func (gw *GraphWalker) GraphWalkImportTypeName(ty *tree.ImportTypeName) WalkResult {
	return gw.GraphWalkType(&ty.Import)
}

func (gw *GraphWalker) GraphWalkTypeApplication(ty *tree.TypeApplication) WalkResult {
	allDeps := make([]WalkResult, len(ty.Args)+1)
	for _, arg := range ty.Args {
		allDeps = append(allDeps, gw.GraphWalkType(&arg))
	}
	allDeps = append(allDeps, gw.GraphWalkType(&ty.Type))
	return CombineWalkResults(allDeps...)
}

func (gw *GraphWalker) GraphWalkInterfaceType(ty *tree.InterfaceType) WalkResult {
	allDeps := make([]WalkResult, 0, len(ty.Methods))
	for _, method := range ty.Methods {
		allDeps = append(allDeps, gw.GraphWalkSignature(method.Type.Signature))
	}
	for _, constraint := range ty.Constraints {
		for _, param := range constraint.TypeElem.Union {
			deps := gw.GraphWalkType(&param.Type)
			allDeps = append(allDeps, deps)
		}
	}
	return CombineWalkResults(allDeps...)
}

func (gw *GraphWalker) GraphWalkStructType(ty *tree.StructType) WalkResult {
	allDeps := make([]WalkResult, 0, len(ty.Fields))
	for _, field := range ty.Fields {
		allDeps = append(allDeps, gw.GraphWalkType(&field.Type))
	}
	return CombineWalkResults(allDeps...)
}

func (gw *GraphWalker) GraphWalkArrayType(ty *tree.ArrayType) WalkResult {
	return CombineWalkResults(
		gw.GraphWalkExpr(&ty.Len),
		gw.GraphWalkType(&ty.ElemType),
	)
}

func (gw *GraphWalker) GraphWalkMapType(ty *tree.MapType) WalkResult {
	return CombineWalkResults(
		gw.GraphWalkType(&ty.KeyType),
		gw.GraphWalkType(&ty.ValueType),
	)
}

func (gw *GraphWalker) GraphWalkExpr(arg *tree.Expr) WalkResult {
	if *arg == nil {
		return EmptyResult()
	}
	switch expr := (*arg).(type) {
	case *tree.NameExpr:
		return NewExprRef(expr.Name, arg)
	case *tree.TypeExpr:
		return gw.GraphWalkType(&expr.Type)
	case *tree.BinaryExpr:
		return CombineWalkResults(
			gw.GraphWalkExpr(&expr.Left),
			gw.GraphWalkExpr(&expr.Right),
		)
	case *tree.UnaryExpr:
		return gw.GraphWalkExpr(&expr.Expr)
	case *tree.StarExpr:
		return gw.GraphWalkExpr(&expr.Expr)
	case *tree.AddressExpr:
		return gw.GraphWalkExpr(&expr.Expr)
	case *tree.ConversionExpr:
		return CombineWalkResults(
			gw.GraphWalkExpr(&expr.Expr),
			gw.GraphWalkType(&expr.Type),
		)
	case *tree.SelectorExpr:
		return gw.GraphWalkExpr(&expr.Expr)
	case *tree.IndexExpr:
		return CombineWalkResults(
			gw.GraphWalkExpr(&expr.Expr),
			gw.GraphWalkExprs(expr.Indices),
		)
	case *tree.SliceExpr:
		return CombineWalkResults(
			gw.GraphWalkExpr(&expr.Expr),
			gw.GraphWalkExpr(&expr.Low),
			gw.GraphWalkExpr(&expr.High),
			gw.GraphWalkExpr(&expr.Max),
		)
	case *tree.TypeSwitchAssertionExpr:
		return CombineWalkResults(
			gw.GraphWalkExpr(&expr.Expr),
		)
	case *tree.TypeAssertionExpr:
		return CombineWalkResults(
			gw.GraphWalkExpr(&expr.Expr),
			gw.GraphWalkType(&expr.Type),
		)
	case *tree.CallExpr:
		allDeps := make([]WalkResult, 0, 1+len(expr.Args))
		allDeps = append(allDeps, gw.GraphWalkExpr(&expr.Func))
		for _, arg := range expr.Args {
			deps := gw.GraphWalkExpr(&arg)
			allDeps = append(allDeps, deps)
		}
		return CombineWalkResults(allDeps...)
	case *tree.LiteralExpr:
		return EmptyResult()
	case *tree.FuncLitExpr:
		return gw.GraphWalkSignature(expr.Signature)
	case *tree.CompositeLitExpr:
		allDeps := make([]WalkResult, 0, 1+len(expr.Elems))
		allDeps = append(allDeps, gw.GraphWalkType(&expr.Type))
		for _, elem := range expr.Elems {
			allDeps = append(allDeps, CombineWalkResults(
				gw.GraphWalkCompositeLitKey(&elem.Key),
				gw.GraphWalkExpr(&elem.Value),
			))
		}
		return CombineWalkResults(allDeps...)
	case *tree.EllipsisExpr:
		return EmptyResult()
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func (gw *GraphWalker) GraphWalkCompositeLitKey(expr *tree.Expr) WalkResult {
	switch ty := (*expr).(type) {
	case *tree.NameExpr:
		// could be a struct field name
		gw.CompositeLitKeys.Add(ty.Name)
		return gw.GraphWalkExpr(expr)
	default:
		return gw.GraphWalkExpr(expr)
	}
}

func (gw *GraphWalker) GraphWalkExprs(exprs []tree.Expr) WalkResult {
	allDeps := make([]WalkResult, 0, len(exprs))
	for i := range exprs {
		allDeps = append(allDeps, gw.GraphWalkExpr(&exprs[i]))
	}
	return CombineWalkResults(allDeps...)
}

// ====================

func VerifyNameCycles(names common.Map[common.Identifier, *NameInfo], dependencies NameDependencies) {
	// only top-level VarDecls & ConstDecls can have cycles
	// all other declarations are part of an implicit LET REC
	var checking = make(map[common.Identifier]*NameInfo)
	for id, name := range names {
		if name.Kind == common.DeclKindVar || name.Kind == common.DeclKindConst {
			checking[id] = name
		}
	}

	cycle := algos.FindCycle(checking, func(name *NameInfo) common.Set[common.Identifier] {
		if name == nil {
			return common.NewSet[common.Identifier]()
		}
		return name.Deps
	})

	if len(cycle) > 0 {
		ns := make([]string, 0, len(cycle))
		for _, name := range cycle {
			ns = append(ns, name.Name.Value)
		}
		panic(fmt.Errorf("cycle in dependencies: %v", strings.Join(ns, " -> ")))
	}
}

// ====================

func ApplyReplacements(
	reps map[common.Identifier]tree.Node,
	nameTypeRefs common.Map[common.Identifier, common.Set[*tree.Type]],
	nameExprRefs common.Map[common.Identifier, common.Set[*tree.Expr]],
) {
	if len(reps) == 0 {
		return
	}
	for name, rep := range reps {
		for ref := range nameTypeRefs[name] {
			*ref = rep.(tree.Type)
		}
		for ref := range nameExprRefs[name] {
			switch rep := rep.(type) {
			case tree.Type:
				*ref = &tree.TypeExpr{Type: rep}
			case tree.Expr:
				*ref = rep
			default:
				panic("unreachable")
			}
		}
		delete(nameTypeRefs, name)
		delete(nameExprRefs, name)
	}
}

func MakeReplacement(pkg *source.Package, name *NameInfo) tree.Node {
	switch name.Kind {
	case common.DeclKindImport:
		return &tree.ImportRef{ImportPath: pkg.ImportPath, ImportDeclaredName: name.Name}
	case common.DeclKindConst, common.DeclKindVar, common.DeclKindFunc:
		return &tree.PackageNameExpr{Path: pkg.ImportPath, Name: name.Name}
	case common.DeclKindType, common.DeclKindAlias:
		return &tree.PackageTypeName{Path: pkg.ImportPath, Name: name.Name}
	default:
		spew.Dump(name.Kind)
		panic("unreachable")
	}
}

// ====================

func TopologicalSort2(nodes common.Map[common.Identifier, *NameInfo], edges func(*NameInfo) common.Set[common.Identifier]) []*NameInfo {
	inDegree := common.Map[common.Identifier, common.Set[*NameInfo]]{}
	for k := range nodes {
		inDegree[k] = common.NewSet[*NameInfo]()
	}
	for _, node := range nodes {
		for dep := range edges(node) {
			if inDegree[dep] == nil {
				panic(fmt.Errorf("missing dependency: %v", dep))
			}
			inDegree[dep].Add(node)
		}
	}

	var queue []common.Identifier
	for k, degree := range inDegree {
		if len(degree) == 0 {
			queue = append(queue, k)
			inDegree.Remove(k)
		}
	}

	var sorted []*NameInfo
	for len(queue) > 0 {
		k := queue[0]
		queue = queue[1:]
		sorted = append(sorted, nodes[k])
		for dep := range edges(nodes[k]) {
			inDegree[dep].Remove(nodes[k])
			if len(inDegree[dep]) == 0 {
				queue = append(queue, dep)
				inDegree.Remove(dep)
			}
		}
	}

	for k, deps := range inDegree {
		if len(deps) == 0 {
			continue
		}
		var ns []string
		for dep := range deps {
			ns = append(ns, dep.Name.Value)
		}
		//panic(fmt.Sprintf("callers: %v -> %v", k, ns))
		sorted = append(sorted, nodes[k]) // TODO :shrug:
	}

	//if len(sorted) != len(nodes) {
	//	panic("cycle in dependencies")
	//}

	slices.Reverse(sorted)

	return sorted
}

// ====================
