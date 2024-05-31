package parse

import (
	"go/ast"
	goparser "go/parser"
	"go/token"
)

type GoParser interface {
	ParseFile(path string) (*ast.File, error)
	ParseDir(path string) (map[string]*ast.Package, error)
}

func NewGoParser() GoParser {
	return &goParser{
		fileSet: token.NewFileSet(),
	}
}

type goParser struct {
	fileSet *token.FileSet
}

func (p *goParser) ParseFile(path string) (*ast.File, error) {
	return goparser.ParseFile(p.fileSet, path, nil, goparser.ParseComments)
}

func (p *goParser) ParseDir(path string) (map[string]*ast.Package, error) {
	return goparser.ParseDir(p.fileSet, path, nil, goparser.ParseComments)
}
