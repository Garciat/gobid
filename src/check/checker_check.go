package check

import (
	"fmt"
	"github.com/davecgh/go-spew/spew"
	"github.com/garciat/gobid/source"
	"github.com/garciat/gobid/tree"
)

func (c *Checker) CheckAssignableTo(sub, super tree.Type) {
	if sub == nil || super == nil {
		panic("nil type")
	}
	c.TyCtx.AddRelation(RelationSubtype{Sub: sub, Super: super})
}

func (c *Checker) CheckEqualTypes(left, right tree.Type) {
	if left == nil || right == nil {
		panic("nil type")
	}
	c.TyCtx.AddRelation(RelationEq{Left: left, Right: right})
}

func (c *Checker) CheckSatisfies(ty tree.Type, constraint *tree.InterfaceType) {
	if ty == nil || constraint == nil {
		panic("nil type")
	}
	c.TyCtx.AddRelation(RelationSatisfies{Type: ty, Constraint: constraint})
}

func (c *Checker) CheckFile(file *source.FileDef) {
	fmt.Printf("=== Checker.CheckFile(%v) ===\n", file.Path)
	for _, decl := range file.Decls {
		c.CheckDecl(decl)
	}
}

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

	scope.CheckTypeDeclType(decl.Type)

	subst := scope.Verify()

	scope.CheckSubst(decl.TypeParams, subst)
}

// CheckTypeDeclType xplores the type declaration, checking if any type applications are valid
func (c *Checker) CheckTypeDeclType(ty tree.Type) {
	switch ty := ty.(type) {
	case *tree.TypeName:
		// nothing to do
	case *tree.TypeBuiltin:
		// nothing to do
	case *tree.TypeParam:
		// nothing to do
	case *tree.PackageTypeName:
		// nothing to do
	case *tree.ImportTypeName:
		// nothing to do
	case *tree.TypeApplication:
		c.TypeApplicationFunc(ty, func(tyParam *tree.TypeParamDecl, tyArg tree.Type) {
			c.CheckTypeDeclType(tyArg)
		})
	case *tree.StructType:
		for _, field := range ty.Fields {
			c.CheckTypeDeclType(field.Type)
		}
	case *tree.InterfaceType:
		for _, m := range ty.Methods {
			c.CheckTypeDeclType(m.Type)
		}
		for _, ctr := range ty.Constraints {
			for _, term := range ctr.TypeElem.Union {
				c.CheckTypeDeclType(term.Type)
			}
		}
	case *tree.FunctionType:
		c.CheckTypeDeclSignature(ty.Signature)
	case *tree.SliceType:
		c.CheckTypeDeclType(ty.ElemType)
	case *tree.NamedType:
		c.CheckTypeDeclType(ty.Type)
	case *tree.PointerType:
		c.CheckTypeDeclType(ty.ElemType)
	case *tree.ArrayType:
		c.CheckTypeDeclType(ty.ElemType)
	case *tree.MapType:
		c.CheckTypeDeclType(ty.KeyType)
		c.CheckTypeDeclType(ty.ValueType)
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
		c.CheckTypeDeclType(param.Type)
	}
	for _, result := range sig.Results.Params {
		c.CheckTypeDeclType(result.Type)
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
		receiverTy = scope.ResolveType(pointerTy.ElemType)
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
	case *tree.DeferStmt:
		c.CheckExpr(stmt.Call, c.BuiltinType("any"))
	case *tree.GoStmt:
		c.CheckExpr(stmt.Call, c.BuiltinType("any"))
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
			c.CheckAssignableTo(c.Synth(result), c.ResolveType(fn.Results.Params[i].Type))
		}
	} else if len(stmt.Results) == 1 && len(fn.Results.Params) > 0 {
		ty := c.Synth(stmt.Results[0])
		switch ty := c.Under(ty).(type) {
		case *tree.TupleType:
			if len(fn.Results.Params) != len(ty.Elems) {
				panic("wrong number of return in tuple expansion")
			}
			for i, param := range fn.Results.Params {
				c.CheckAssignableTo(ty.Elems[i], c.ResolveType(param.Type))
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
	ifScope := c.BeginScope(ScopeKindBlock)
	if stmt.Init != nil {
		ifScope.CheckStatement(stmt.Init)
	}
	if stmt.Cond != nil {
		// TODO only allowed for else
		ifScope.CheckExpr(stmt.Cond, c.BuiltinType("bool"))
	}
	ifScope.CheckStatementList(stmt.Body)
	if stmt.Else != nil {
		ifScope.CheckStatement(stmt.Else)
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

	switch ty := scope.Under(targetTy).(type) {
	case *tree.PointerType:
		targetTy = c.ResolveType(ty.ElemType)
	}

	var keyTy, valueTy tree.Type

	switch targetTy := scope.Under(targetTy).(type) {
	case *tree.TypeBuiltin:
		if targetTy.Name.Value == "string" {
			keyTy = scope.BuiltinType("int")
			valueTy = scope.BuiltinType("rune")
		}
	case *tree.SliceType:
		keyTy = scope.BuiltinType("int")
		valueTy = scope.ResolveType(targetTy.ElemType)
	case *tree.MapType:
		keyTy = scope.ResolveType(targetTy.KeyType)
		valueTy = scope.ResolveType(targetTy.ValueType)
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
	switchScope := c.BeginScope(ScopeKindBlock)
	var tagTy tree.Type
	if stmt.Init != nil {
		switchScope.CheckStatement(stmt.Init)
	}
	if stmt.Tag != nil {
		tagTy = switchScope.Synth(stmt.Tag)
	}
	for _, caseStmt := range stmt.Cases {
		caseScope := switchScope.BeginScope(ScopeKindBlock)
		for _, expr := range caseStmt.Exprs {
			if tagTy != nil {
				caseScope.CheckExpr(expr, tagTy)
			} else {
				caseScope.CheckExpr(expr, switchScope.BuiltinType("bool"))
			}
		}
		caseScope.CheckStatementList(caseStmt.Body)
	}
}

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
	c.CheckAssignableTo(c.Synth(expr), c.ResolveType(ty))
}

func (c *Checker) CheckPackageNameExpr(expr *tree.PackageNameExpr, ty tree.Type) {
	c.CheckAssignableTo(c.Synth(expr), c.ResolveType(ty))
}

func (c *Checker) CheckBinaryExpr(expr *tree.BinaryExpr, ty tree.Type) {
	c.CheckAssignableTo(c.Synth(expr), c.ResolveType(ty))
}

func (c *Checker) CheckUnaryExpr(expr *tree.UnaryExpr, ty tree.Type) {
	c.CheckAssignableTo(c.Synth(expr), c.ResolveType(ty))
}

func (c *Checker) CheckSelectorExpr(expr *tree.SelectorExpr, ty tree.Type) {
	c.CheckAssignableTo(c.Synth(expr), c.ResolveType(ty))
}

func (c *Checker) CheckIndexExpr(expr *tree.IndexExpr, ty tree.Type) {
	c.CheckAssignableTo(c.Synth(expr), c.ResolveType(ty))
}

func (c *Checker) CheckCallExpr(expr *tree.CallExpr, ty tree.Type) {
	c.CheckAssignableTo(c.Synth(expr), c.ResolveType(ty))
}

func (c *Checker) CheckLiteralExpr(expr *tree.LiteralExpr, ty tree.Type) {
	c.CheckAssignableTo(c.Synth(expr), c.ResolveType(ty))
}

func (c *Checker) CheckCompositeLitExpr(expr *tree.CompositeLitExpr, ty tree.Type) {
	var exprTy tree.Type
	if expr.Type == nil {
		exprTy = c.MakeCompositeLit(expr, c.ResolveType(ty))
	} else {
		exprTy = c.Synth(expr)
	}
	c.CheckAssignableTo(exprTy, c.ResolveType(ty))
}

func (c *Checker) CheckFuncLitExpr(expr *tree.FuncLitExpr, ty tree.Type) {
	c.CheckAssignableTo(c.Synth(expr), c.ResolveType(ty))
}
