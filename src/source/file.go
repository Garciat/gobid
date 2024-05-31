package source

import (
	"github.com/garciat/gobid/common"
	"github.com/garciat/gobid/tree"
)

type FileDef struct {
	Path            string
	PackageName     string
	BuildConstraint string
	Imports         []common.ImportPath
	Decls           []tree.Decl
}
