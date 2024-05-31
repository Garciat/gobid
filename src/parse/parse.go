package parse

import (
	"fmt"
	"github.com/garciat/gobid/source"
	"go/ast"
	goparser "go/parser"
	"go/token"
	"slices"
	"strings"

	"github.com/davecgh/go-spew/spew"
	. "github.com/garciat/gobid/common"
	"github.com/garciat/gobid/tree"
)

type Parser interface {
	ParseFile(path string) *source.FileDef
	ParsePackage(path string) []*source.FileDef
}

func NewParser() Parser {
	return &parser{
		gop: NewGoParser(),
	}
}

type parser struct {
	gop GoParser
}

func (p *parser) ParseFile(path string) *source.FileDef {
	fast, err := p.gop.ParseFile(path)
	if err != nil {
		panic(err)
	}
	return p.readAST(path, fast)
}

func (p *parser) ParsePackage(path string) []*source.FileDef {
	packages, err := p.gop.ParseDir(path)
	if err != nil {
		panic(err)
	}

	var files []*source.FileDef

	for _, pkg := range packages {
		for path, f := range pkg.Files {
			var buildConstraint string
			for _, grp := range f.Comments {
				for _, cmt := range grp.List {
					if strings.HasPrefix(cmt.Text, "//go:build") {
						buildConstraint = cmt.Text
					}
				}
			}
			file := p.readAST(path, f)
			file.BuildConstraint = buildConstraint
			files = append(files, file)
		}
	}

	return files
}

func (p *parser) readAST(path string, fast *ast.File) *source.FileDef {
	fmt.Printf("=== Parser.readAST(%v) ===\n", path)
	return ReadFile(path, fast)
}

func ReadFile(path string, file *ast.File) *source.FileDef {
	var imports []ImportPath
	var decls []tree.Decl
	for _, spec := range file.Imports {
		imports = append(imports, ReadImportPath(spec))
	}
	for _, decl := range file.Decls {
		decls = append(decls, ReadDecl(decl)...)
	}
	return &source.FileDef{
		Path:        path,
		PackageName: file.Name.Name,
		Imports:     imports,
		Decls:       decls,
	}
}

func ReadImportPath(spec *ast.ImportSpec) ImportPath {
	path := spec.Path.Value
	path = path[1 : len(path)-1] // remove quotes
	return ImportPath(path)
}

func ReadDecl(decl ast.Decl) []tree.Decl {
	switch decl := decl.(type) {
	case *ast.GenDecl:
		return ReadGenDecl(decl)
	case *ast.FuncDecl:
		return ReadFuncDecl(decl)
	default:
		panic("unreachable")
	}
}

func ReadGenDecl(decl *ast.GenDecl) []tree.Decl {
	switch decl.Tok {
	case token.CONST:
		return ReadConstDecl(decl)
	case token.TYPE:
		return ReadTypeDecl(decl)
	case token.VAR:
		return ReadVarDecl(decl)
	case token.IMPORT:
		return ReadImportDecl(decl)
	default:
		spew.Dump(decl)
		panic("unreachable")
	}
}

func ReadImportDecl(decl *ast.GenDecl) []tree.Decl {
	var decls []tree.Decl
	for _, spec := range decl.Specs {
		spec := spec.(*ast.ImportSpec)
		var name *Identifier
		if spec.Name != nil {
			name = Ptr(NewIdentifier(spec.Name.Name))
		}
		decls = append(decls, &tree.ImportDecl{
			ImportPath: ReadImportPath(spec),
			Alias:      name,
		})
	}
	return decls
}

func ReadConstDecl(decl *ast.GenDecl) []tree.Decl {
	var elems []tree.ConstDeclElem
	for _, spec := range decl.Specs {
		spec := spec.(*ast.ValueSpec)
		for i, name := range spec.Names {
			var value tree.Expr
			if spec.Values != nil {
				value = ReadExpr(spec.Values[i])
			}
			var declTy tree.Type
			if spec.Type != nil {
				declTy = ReadType(spec.Type)
			}
			elems = append(elems, tree.ConstDeclElem{
				Name:  NewIdentifier(name.Name),
				Type:  declTy,
				Value: value,
			})
		}
	}
	return []tree.Decl{&tree.ConstDecl{Elems: elems}}
}

func ReadTypeDecl(decl *ast.GenDecl) []tree.Decl {
	var decls []tree.Decl
	for _, spec := range decl.Specs {
		spec := spec.(*ast.TypeSpec)
		decls = append(decls, &tree.TypeDecl{
			Name:       NewIdentifier(spec.Name.Name),
			TypeParams: ReadTypeParamList(spec.TypeParams),
			Type:       ReadType(spec.Type),
		})
	}
	return decls
}

func ReadVarDecl(decl *ast.GenDecl) []tree.Decl {
	var decls []tree.Decl
	for _, spec := range decl.Specs {
		spec := spec.(*ast.ValueSpec)
		names := []Identifier{}
		exprs := []tree.Expr{}
		for _, name := range spec.Names {
			names = append(names, NewIdentifier(name.Name))
		}
		for _, expr := range spec.Values {
			exprs = append(exprs, ReadExpr(expr))
		}
		var declTy tree.Type
		if spec.Type != nil {
			declTy = ReadType(spec.Type)
		}
		decls = append(decls, &tree.VarDecl{
			Names: names,
			Type:  declTy,
			Exprs: exprs,
		})
	}
	return decls
}

func ReadFuncDecl(decl *ast.FuncDecl) []tree.Decl {
	if decl.Recv == nil {
		return []tree.Decl{&tree.FunctionDecl{
			Name:      NewIdentifier(decl.Name.Name),
			Signature: ReadSignature(decl.Type),
			Body:      ReadStatementList(decl.Body),
		}}
	} else {
		return []tree.Decl{&tree.MethodDecl{
			Receiver:  ReadReceiver(decl.Recv.List[0]),
			Name:      NewIdentifier(decl.Name.Name),
			Signature: ReadSignature(decl.Type),
			Body:      ReadStatementList(decl.Body),
		}}

	}
}

func ReadReceiver(field *ast.Field) tree.FieldDecl {
	switch len(field.Names) {
	case 0:
		return tree.FieldDecl{
			Name: IgnoreIdent,
			Type: ReadType(field.Type),
		}
	case 1:
		return tree.FieldDecl{
			Name: NewIdentifier(field.Names[0].Name),
			Type: ReadType(field.Type),
		}
	default:
		panic("too many names for receiver declaration")
	}
}

func ReadTypeParamList(list *ast.FieldList) tree.TypeParamList {
	if list == nil {
		return tree.TypeParamList{}
	}
	var params []tree.TypeParamDecl
	for _, field := range list.List {
		for _, name := range field.Names {
			params = append(params, tree.TypeParamDecl{
				Name:       NewIdentifier(name.Name),
				Constraint: ReadTypeConstraint(field.Type),
			})
		}
	}
	return tree.TypeParamList{Params: params}
}

func ReadTypeConstraint(expr ast.Expr) *tree.InterfaceType {
	switch expr := expr.(type) {
	case *ast.Ident:
		return &tree.InterfaceType{
			Methods: nil,
			Constraints: []tree.TypeConstraint{
				{
					TypeElem: tree.TypeElem{
						Union: []tree.TypeTerm{
							{Type: &tree.TypeName{Name: NewIdentifier(expr.Name)}},
						},
					},
				},
			},
		}
	case *ast.BinaryExpr:
		if expr.Op != token.OR {
			panic("Expected OR")
		}
		terms := []tree.TypeTerm{}
		cur := expr
		for {
			terms = append(terms, ReadUnionTerm(cur.Y))
			if next, ok := cur.X.(*ast.BinaryExpr); ok {
				cur = next
			} else {
				terms = append(terms, ReadUnionTerm(cur.X))
				break
			}
		}
		slices.Reverse(terms)
		return &tree.InterfaceType{
			Methods: nil,
			Constraints: []tree.TypeConstraint{
				{
					TypeElem: tree.TypeElem{
						Union: terms,
					},
				},
			},
		}
	case *ast.InterfaceType:
		return ReadInterfaceType(expr)
	default:
		return &tree.InterfaceType{
			Methods: nil,
			Constraints: []tree.TypeConstraint{
				{
					TypeElem: tree.TypeElem{
						Union: []tree.TypeTerm{
							ReadUnionTerm(expr),
						},
					},
				},
			},
		}
	}
}

func ReadUnionTerm(expr ast.Expr) tree.TypeTerm {
	switch expr := expr.(type) {
	case *ast.UnaryExpr:
		if expr.Op != token.TILDE {
			panic("Expected TILDE")
		}
		return tree.TypeTerm{Type: ReadType(expr.X), Tilde: true}
	default:
		return tree.TypeTerm{Type: ReadType(expr)}
	}
}

func ReadSignature(sig *ast.FuncType) tree.Signature {
	return tree.Signature{
		TypeParams: ReadTypeParamList(sig.TypeParams),
		Params:     ReadParameterList(sig.Params),
		Results:    ReadResultsList(sig.Results),
	}
}

func ReadParameterList(list *ast.FieldList) tree.ParameterList {
	if list == nil {
		return tree.ParameterList{}
	}

	var params []tree.ParameterDecl
	var foundVariadic bool = false

	addParam := func(name Identifier, fieldType ast.Expr) {
		if foundVariadic {
			panic("variadic parameter must be last")
		}
		var ty tree.Type
		var variadic bool
		if ellipsis, ok := fieldType.(*ast.Ellipsis); ok {
			ty = ReadType(ellipsis.Elt)
			variadic = true
			foundVariadic = true
		} else {
			ty = ReadType(fieldType)
			variadic = false
		}
		params = append(params, tree.ParameterDecl{
			Name:     name,
			Type:     ty,
			Variadic: variadic,
		})
	}

	for _, field := range list.List {
		if len(field.Names) == 0 {
			addParam(IgnoreIdent, field.Type)
		}
		for _, name := range field.Names {
			addParam(NewIdentifier(name.Name), field.Type)
		}
	}
	return tree.ParameterList{Params: params}
}

func ReadResultsList(list *ast.FieldList) tree.ParameterList {
	if list == nil {
		return tree.ParameterList{}
	}
	var params []tree.ParameterDecl
	for _, field := range list.List {
		if len(field.Names) == 0 {
			params = append(params, tree.ParameterDecl{
				Name: IgnoreIdent,
				Type: ReadType(field.Type),
			})
		}
		for _, name := range field.Names {
			params = append(params, tree.ParameterDecl{
				Name: NewIdentifier(name.Name),
				Type: ReadType(field.Type),
			})
		}
	}
	return tree.ParameterList{Params: params}
}

func ReadBlockStmt(block *ast.BlockStmt) tree.Statement {
	return &tree.BlockStmt{Body: ReadStatementList(block)}
}

func ReadStatementList(block *ast.BlockStmt) tree.StatementList {
	if block == nil {
		return tree.StatementList{}
	}
	var stmts []tree.Statement
	for _, stmt := range block.List {
		stmts = append(stmts, ReadStmt(stmt))
	}
	return tree.StatementList{Stmts: stmts}
}

func ReadStmt(stmt ast.Stmt) tree.Statement {
	switch stmt := stmt.(type) {
	case *ast.DeclStmt:
		return ReadDeclStmt(stmt)
	case *ast.ExprStmt:
		return ReadExprStmt(stmt)
	case *ast.ReturnStmt:
		return ReadReturnStmt(stmt)
	case *ast.IfStmt:
		return ReadIfStmt(stmt)
	case *ast.AssignStmt:
		return ReadAssignStmt(stmt)
	case *ast.EmptyStmt:
		return &tree.EmptyStmt{}
	case *ast.RangeStmt:
		return ReadRangeStmt(stmt)
	case *ast.IncDecStmt:
		return ReadIncDecStmt(stmt)
	case *ast.TypeSwitchStmt:
		return ReadTypeSwitchStmt(stmt)
	case *ast.SwitchStmt:
		return ReadSwitchStmt(stmt)
	case *ast.BranchStmt:
		return ReadBranchStmt(stmt)
	case *ast.ForStmt:
		return ReadForStmt(stmt)
	case *ast.GoStmt:
		return &tree.GoStmt{Call: ReadCallExpr(stmt.Call).(*tree.CallExpr)}
	case *ast.DeferStmt:
		return &tree.DeferStmt{Call: ReadCallExpr(stmt.Call).(*tree.CallExpr)}
	case *ast.LabeledStmt:
		// TODO labeled satement semantics?
		return ReadStmt(stmt.Stmt)
	case *ast.SendStmt:
		if stmt.Value == nil {
			return &tree.ReceiveStmt{
				Chan: ReadExpr(stmt.Chan),
			}
		} else {
			return &tree.SendStmt{
				Chan:  ReadExpr(stmt.Chan),
				Value: ReadExpr(stmt.Value),
			}
		}
	case *ast.SelectStmt:
		return &tree.SelectStmt{Cases: ReadCommCases(stmt.Body.List)}
	case *ast.BlockStmt:
		return ReadBlockStmt(stmt)
	default:
		spew.Dump(stmt)
		panic("unreachable")
	}
}

func ReadStmtList(stmts []ast.Stmt) tree.StatementList {
	var result []tree.Statement
	for _, stmt := range stmts {
		result = append(result, ReadStmt(stmt))
	}
	return tree.StatementList{Stmts: result}
}

func ReadDeclStmt(stmt *ast.DeclStmt) tree.Statement {
	return &tree.DeclStmt{Decl: ReadDecl(stmt.Decl)[0]}
}

func ReadExprStmt(stmt *ast.ExprStmt) tree.Statement {
	return &tree.ExpressionStmt{Expr: ReadExpr(stmt.X)}
}

func ReadReturnStmt(stmt *ast.ReturnStmt) tree.Statement {
	var results []tree.Expr
	for _, result := range stmt.Results {
		results = append(results, ReadExpr(result))
	}
	return &tree.ReturnStmt{Results: results}
}

func ReadIfStmt(stmt *ast.IfStmt) *tree.IfStmt {
	var init tree.Statement
	if stmt.Init != nil {
		init = ReadStmt(stmt.Init)
	}
	var elseStmt *tree.IfStmt
	if stmt.Else != nil {
		elseStmt = ReadElseStmt(stmt.Else)
	}
	return &tree.IfStmt{
		Init: init,
		Cond: ReadExpr(stmt.Cond),
		Body: ReadStatementList(stmt.Body),
		Else: elseStmt,
	}
}

func ReadElseStmt(stmt ast.Stmt) *tree.IfStmt {
	switch stmt := stmt.(type) {
	case *ast.BlockStmt:
		return &tree.IfStmt{
			Cond: nil,
			Body: ReadStatementList(stmt),
		}
	case *ast.IfStmt:
		return ReadIfStmt(stmt)
	default:
		spew.Dump(stmt)
		panic("unreachable")
	}
}

func ReadAssignStmt(stmt *ast.AssignStmt) tree.Statement {
	if stmt.Tok == token.DEFINE {
		names := []Identifier{}
		exprs := []tree.Expr{}
		for _, left := range stmt.Lhs {
			switch left := left.(type) {
			case *ast.Ident:
				names = append(names, NewIdentifier(left.Name))
			default:
				panic("TODO")
			}
		}
		for _, right := range stmt.Rhs {
			exprs = append(exprs, ReadExpr(right))
		}
		return &tree.ShortVarDecl{Names: names, Exprs: exprs}
	} else {
		var lhs []tree.Expr
		var rhs []tree.Expr
		for _, left := range stmt.Lhs {
			lhs = append(lhs, ReadExpr(left))
		}
		for _, right := range stmt.Rhs {
			rhs = append(rhs, ReadExpr(right))
		}
		return &tree.AssignmentStmt{LHS: lhs, RHS: rhs}
	}
}

func ReadRangeStmt(stmt *ast.RangeStmt) tree.Statement {
	var key, value tree.Expr
	if stmt.Key != nil {
		key = ReadExpr(stmt.Key)
	}
	if stmt.Value != nil {
		value = ReadExpr(stmt.Value)
	}
	return &tree.RangeStmt{
		Assign: stmt.Tok == token.DEFINE,
		Key:    key,
		Value:  value,
		X:      ReadExpr(stmt.X),
		Body:   ReadStatementList(stmt.Body),
	}
}

func ReadIncDecStmt(stmt *ast.IncDecStmt) tree.Statement {
	return &tree.IncDecStmt{
		Expr: ReadExpr(stmt.X),
		Inc:  stmt.Tok == token.INC,
	}
}

func ReadCommCases(clauses []ast.Stmt) []tree.SelectCase {
	var cases []tree.SelectCase
	for _, clause := range clauses {
		clause := clause.(*ast.CommClause)
		var comm tree.Statement
		if clause.Comm != nil {
			comm = ReadStmt(clause.Comm)
		}
		cases = append(cases, tree.SelectCase{
			Comm: comm,
			Body: ReadStmtList(clause.Body),
		})
	}
	return cases
}

func ReadTypeSwitchStmt(stmt *ast.TypeSwitchStmt) tree.Statement {
	var init tree.Statement
	if stmt.Init != nil {
		init = ReadStmt(stmt.Init)
	}
	return &tree.TypeSwitchStmt{
		Init:   init,
		Assign: ReadTypeSwitchAssign(stmt.Assign),
		Body:   ReadTypeSwitchCases(stmt.Body.List),
	}
}

func ReadTypeSwitchAssign(stmt ast.Stmt) tree.Statement {
	switch stmt := stmt.(type) {
	case *ast.AssignStmt:
		if stmt.Tok != token.DEFINE {
			panic("expected define")
		}
		if len(stmt.Lhs) != 1 {
			panic("expected single lhs")
		}
		if len(stmt.Rhs) != 1 {
			panic("expected single rhs")
		}
		return &tree.ShortVarDecl{
			Names: []Identifier{NewIdentifier(stmt.Lhs[0].(*ast.Ident).Name)},
			Exprs: []tree.Expr{ReadTypeSwitchTypeAssert(stmt.Rhs[0])},
		}
	case *ast.ExprStmt:
		return &tree.ExpressionStmt{Expr: ReadTypeSwitchTypeAssert(stmt.X)}
	default:
		spew.Dump(stmt)
		panic("unreachable")
	}
}

func ReadTypeSwitchTypeAssert(expr ast.Expr) tree.Expr {
	switch expr := expr.(type) {
	case *ast.TypeAssertExpr:
		if expr.Type != nil {
			panic("type switch type assert with type")
		}
		return &tree.TypeSwitchAssertionExpr{
			Expr: ReadExpr(expr.X),
		}
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func ReadTypeSwitchCases(clauses []ast.Stmt) []tree.TypeSwitchCase {
	var cases []tree.TypeSwitchCase
	for _, clause := range clauses {
		clause := clause.(*ast.CaseClause)
		var types []tree.Type
		for _, expr := range clause.List {
			types = append(types, ReadType(expr))
		}
		cases = append(cases, tree.TypeSwitchCase{
			Types: types,
			Body:  ReadStmtList(clause.Body),
		})
	}
	return cases
}

func ReadSwitchStmt(stmt *ast.SwitchStmt) tree.Statement {
	var init tree.Statement
	var tag tree.Expr
	if stmt.Init != nil {
		init = ReadStmt(stmt.Init)
	}
	if stmt.Tag != nil {
		tag = ReadExpr(stmt.Tag)
	}
	return &tree.SwitchStmt{
		Init:  init,
		Tag:   tag,
		Cases: ReadSwitchCases(stmt.Body.List),
	}
}

func ReadSwitchCases(clauses []ast.Stmt) []tree.SwitchCase {
	var cases []tree.SwitchCase
	for _, clause := range clauses {
		clause := clause.(*ast.CaseClause)
		var exprs []tree.Expr
		for _, expr := range clause.List {
			exprs = append(exprs, ReadExpr(expr))
		}
		cases = append(cases, tree.SwitchCase{
			Exprs: exprs,
			Body:  ReadStmtList(clause.Body),
		})
	}
	return cases
}

func ReadBranchStmt(stmt *ast.BranchStmt) tree.Statement {
	// TOOD matters?
	switch stmt.Tok {
	case token.BREAK:
		return &tree.BranchStmt{}
	case token.CONTINUE:
		return &tree.BranchStmt{}
	case token.FALLTHROUGH:
		return &tree.BranchStmt{}
	case token.GOTO:
		return &tree.BranchStmt{}
	default:
		panic("unreachable")
	}
}

func ReadForStmt(stmt *ast.ForStmt) tree.Statement {
	var init, post tree.Statement
	var cond tree.Expr
	if stmt.Init != nil {
		init = ReadStmt(stmt.Init)
	}
	if stmt.Post != nil {
		post = ReadStmt(stmt.Post)
	}
	if stmt.Cond != nil {
		cond = ReadExpr(stmt.Cond)
	}
	return &tree.ForStmt{
		Init: init,
		Cond: cond,
		Post: post,
		Body: ReadStatementList(stmt.Body),
	}
}

func ReadExpr(expr ast.Expr) tree.Expr {
	if expr == nil {
		panic("NOPE")
	}
	switch expr := expr.(type) {
	case *ast.BinaryExpr:
		return ReadBinaryExpr(expr)
	case *ast.UnaryExpr:
		return ReadUnaryExpr(expr)
	case *ast.CallExpr:
		return ReadCallExpr(expr)
	case *ast.Ident:
		return ReadNameExpr(expr)
	case *ast.BasicLit:
		return &tree.LiteralExpr{Literal: ReadLiteral(expr)}
	case *ast.IndexExpr:
		return &tree.IndexExpr{
			Expr:    ReadExpr(expr.X),
			Indices: []tree.Expr{ReadExpr(expr.Index)},
		}
	case *ast.IndexListExpr:
		return &tree.IndexExpr{
			Expr:    ReadExpr(expr.X),
			Indices: ReadExprList(expr.Indices),
		}
	case *ast.StarExpr:
		return &tree.UnaryExpr{
			Op:   tree.UnaryOpDeref,
			Expr: ReadExpr(expr.X),
		}
	case *ast.ParenExpr:
		return ReadExpr(expr.X)
	case *ast.CompositeLit:
		return ReadCompositeLit(expr)
	case *ast.SelectorExpr:
		return &tree.SelectorExpr{
			Expr: ReadExpr(expr.X),
			Sel:  NewIdentifier(expr.Sel.Name),
		}
	case *ast.TypeAssertExpr:
		if expr.Type == nil {
			panic("cannot .(type) outside type switch")
		}
		return &tree.TypeAssertionExpr{
			Expr: ReadExpr(expr.X),
			Type: ReadType(expr.Type),
		}
	case *ast.SliceExpr:
		var low, high, max tree.Expr
		if expr.Low != nil {
			low = ReadExpr(expr.Low)
		}
		if expr.High != nil {
			high = ReadExpr(expr.High)
		}
		if expr.Max != nil {
			max = ReadExpr(expr.Max)
		}
		return &tree.SliceExpr{
			Expr: ReadExpr(expr.X),
			Low:  low,
			High: high,
			Max:  max,
		}
	case *ast.FuncLit:
		return ReadFuncLit(expr)
	case *ast.Ellipsis:
		return &tree.EllipsisExpr{}
	default:
		ty, err := Try(func() tree.Expr {
			return &tree.TypeExpr{Type: ReadType(expr)}
		})
		if err == nil {
			return ty
		}
		spew.Dump(err)
		spew.Dump(expr)
		panic("unreachable")
	}
}

func ReadExprList(exprs []ast.Expr) []tree.Expr {
	var result []tree.Expr
	for _, expr := range exprs {
		result = append(result, ReadExpr(expr))
	}
	return result
}

func ReadBinaryExpr(expr *ast.BinaryExpr) tree.Expr {
	return &tree.BinaryExpr{
		Op:    ReadBinaryOp(expr.Op),
		Left:  ReadExpr(expr.X),
		Right: ReadExpr(expr.Y),
	}
}

func ReadUnaryExpr(expr *ast.UnaryExpr) tree.Expr {
	return &tree.UnaryExpr{
		Op:   ReadUnaryOp(expr.Op),
		Expr: ReadExpr(expr.X),
	}
}

func ReadFuncLit(expr *ast.FuncLit) tree.Expr {
	return &tree.FuncLitExpr{
		Signature: ReadSignature(expr.Type),
		Body:      ReadStatementList(expr.Body),
	}
}

func ReadBinaryOp(op token.Token) tree.BinaryOp {
	switch op {
	case token.ADD:
		return tree.BinaryOpAdd
	case token.SUB:
		return tree.BinaryOpSub
	case token.MUL:
		return tree.BinaryOpMul
	case token.QUO:
		return tree.BinaryOpQuo
	case token.REM:
		return tree.BinaryOpRem
	case token.EQL:
		return tree.BinaryOpEq
	case token.NEQ:
		return tree.BinaryOpNeq
	case token.LSS:
		return tree.BinaryOpLt
	case token.LEQ:
		return tree.BinaryOpLte
	case token.GTR:
		return tree.BinaryOpGt
	case token.GEQ:
		return tree.BinaryOpGte
	case token.AND:
		return tree.BinaryOpAnd
	case token.OR:
		return tree.BinaryOpOr
	case token.XOR:
		return tree.BinaryOpXor
	case token.AND_NOT:
		return tree.BinaryOpAndNot
	case token.LAND:
		return tree.BinaryOpLAnd
	case token.LOR:
		return tree.BinaryOpLOr
	case token.SHL:
		return tree.BinaryOpShl
	case token.SHR:
		return tree.BinaryOpShr
	case token.ARROW:
		return tree.BinaryOpArrow
	default:
		spew.Dump(op)
		panic("unreachable")
	}
}

func ReadUnaryOp(op token.Token) tree.UnaryOp {
	switch op {
	case token.ADD:
		return tree.UnaryOpPos
	case token.SUB:
		return tree.UnaryOpNeg
	case token.NOT:
		return tree.UnaryOpNot
	case token.AND:
		return tree.UnaryOpAddr
	case token.MUL:
		return tree.UnaryOpDeref
	case token.ARROW:
		return tree.UnaryOpArrow
	case token.XOR:
		return tree.UnaryOpBitNot
	default:
		spew.Dump(op)
		panic("unreachable")
	}
}

func ReadCallExpr(expr *ast.CallExpr) tree.Expr {
	var args []tree.Expr
	for _, arg := range expr.Args {
		args = append(args, ReadExpr(arg))
	}
	return &tree.CallExpr{
		Func: ReadExpr(expr.Fun),
		Args: args,
	}
}

func ReadNameExpr(expr *ast.Ident) tree.Expr {
	return &tree.NameExpr{Name: NewIdentifier(expr.Name)}
}

func ReadCompositeLit(expr *ast.CompositeLit) tree.Expr {
	var compositeTy tree.Type
	if expr.Type != nil {
		compositeTy = ReadType(expr.Type)
	}
	return &tree.CompositeLitExpr{
		Type:  compositeTy,
		Elems: ReadCompositeLitElems(expr.Elts),
	}
}

func ReadCompositeLitElems(exprs []ast.Expr) []tree.CompositeLitElem {
	var elems []tree.CompositeLitElem
	for _, expr := range exprs {
		switch expr := expr.(type) {
		case *ast.KeyValueExpr:
			elems = append(elems, tree.CompositeLitElem{
				Key:   ReadExpr(expr.Key),
				Value: ReadExpr(expr.Value),
			})
		default:
			elems = append(elems, tree.CompositeLitElem{
				Key:   nil,
				Value: ReadExpr(expr),
			})
		}
	}
	return elems
}

func ReadLiteral(lit *ast.BasicLit) tree.Literal {
	switch lit.Kind {
	case token.INT:
		return &tree.LiteralInt{Value: lit.Value}
	case token.STRING:
		return &tree.LiteralString{Value: lit.Value}
	case token.CHAR:
		return &tree.LiteralRune{Value: lit.Value}
	case token.FLOAT:
		return &tree.LiteralFloat{Value: lit.Value}
	case token.IMAG:
		return &tree.LiteralImag{Value: lit.Value}
	default:
		spew.Dump(lit)
		panic("unreachable")
	}
}

func ReadType(expr ast.Expr) tree.Type {
	if expr == nil {
		panic("NOPE")
	}
	switch expr := expr.(type) {
	case *ast.Ident:
		return &tree.TypeName{Name: NewIdentifier(expr.Name)}
	case *ast.ArrayType:
		if expr.Len != nil {
			return &tree.ArrayType{
				ElemType: ReadType(expr.Elt),
				Len:      ReadExpr(expr.Len),
			}
		} else {
			return &tree.SliceType{ElemType: ReadType(expr.Elt)}
		}
	case *ast.StructType:
		fields := []tree.FieldDecl{}
		for _, field := range expr.Fields.List {
			for _, name := range field.Names {
				fields = append(fields, tree.FieldDecl{
					Name: NewIdentifier(name.Name),
					Type: ReadType(field.Type),
				})
			}
		}
		return &tree.StructType{Fields: fields}
	case *ast.IndexExpr:
		return &tree.TypeApplication{
			ID:   ReadQualIdentifier(expr.X),
			Args: []tree.Type{ReadType(expr.Index)},
		}
	case *ast.IndexListExpr:
		return &tree.TypeApplication{
			ID:   ReadQualIdentifier(expr.X),
			Args: ReadTypeList(expr.Indices),
		}
	case *ast.StarExpr:
		return &tree.PointerType{BaseType: ReadType(expr.X)}
	case *ast.InterfaceType:
		return ReadInterfaceType(expr)
	case *ast.FuncType:
		return &tree.FunctionType{
			Signature: ReadSignature(expr),
		}
	case *ast.MapType:
		return &tree.MapType{
			KeyType:  ReadType(expr.Key),
			ElemType: ReadType(expr.Value),
		}
	case *ast.ChanType:
		return &tree.ChannelType{
			Dir:      ReadChanDir(expr.Dir),
			ElemType: ReadType(expr.Value),
		}
	case *ast.SelectorExpr:
		return &tree.QualIdentifier{
			Package: expr.X.(*ast.Ident).Name,
			Name:    NewIdentifier(expr.Sel.Name),
		}
	case *ast.ParenExpr:
		return ReadType(expr.X)
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func ReadQualIdentifier(expr ast.Expr) tree.QualIdentifier {
	switch expr := expr.(type) {
	case *ast.Ident:
		return tree.QualIdentifier{
			Package: "",
			Name:    NewIdentifier(expr.Name),
		}
	case *ast.SelectorExpr:
		return tree.QualIdentifier{
			Package: expr.X.(*ast.Ident).Name,
			Name:    NewIdentifier(expr.Sel.Name),
		}
	default:
		spew.Dump(expr)
		panic("unreachable")
	}
}

func ReadTypeList(exprs []ast.Expr) []tree.Type {
	var result []tree.Type
	for _, expr := range exprs {
		result = append(result, ReadType(expr))
	}
	return result
}

func ReadChanDir(dir ast.ChanDir) tree.ChannelDir {
	switch dir {
	case ast.SEND:
		return tree.ChannelDirSend
	case ast.RECV:
		return tree.ChannelDirRecv
	case ast.SEND | ast.RECV:
		return tree.ChannelDirBoth
	default:
		panic("unreachable")
	}
}

func ReadInterfaceType(expr *ast.InterfaceType) *tree.InterfaceType {
	methods := []tree.MethodElem{}
	constraints := []tree.TypeConstraint{}
	for _, field := range expr.Methods.List {
		switch len(field.Names) {
		case 0:
			// embedded or constraint
			constraints = append(constraints, tree.TypeConstraint{
				TypeElem: tree.TypeElem{
					Union: []tree.TypeTerm{
						{
							Type: ReadTypeConstraint(field.Type),
						},
					},
				},
			})
		case 1:
			switch ty := ReadType(field.Type).(type) {
			case *tree.FunctionType:
				methods = append(methods, tree.MethodElem{
					Name: NewIdentifier(field.Names[0].Name),
					Type: ty,
				})
			default:
				panic("unreachable")
			}
		default:
			panic("unreachable")
		}
	}
	return &tree.InterfaceType{Methods: methods, Constraints: constraints}
}

// ========================

func ParseExpr(src string) tree.Expr {
	expr, err := goparser.ParseExpr(src)
	if err != nil {
		panic(err)
	}
	return ReadExpr(expr)
}

func ParseType(src string) tree.Type {
	expr, err := goparser.ParseExpr(src)
	if err != nil {
		panic(err)
	}
	return ReadType(expr)
}

func ParseFuncType(src string) *tree.FunctionType {
	expr, err := goparser.ParseExpr(src)
	if err != nil {
		panic(err)
	}
	return ReadType(expr).(*tree.FunctionType)
}
