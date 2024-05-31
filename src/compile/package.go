package compile

import (
	"github.com/garciat/gobid/check"
	. "github.com/garciat/gobid/common"
	"github.com/garciat/gobid/tree"
)

type Package struct {
	ImportPath   ImportPath
	Dependencies map[ImportPath]struct{}
	Name         string
	Files        []*tree.FileDef
	Checker      *check.Checker
}

func NewPackage(ip ImportPath) *Package {
	return &Package{
		ImportPath:   ip,
		Dependencies: make(map[ImportPath]struct{}),
		Name:         ip.PackageName(),
		Files:        nil,
		Checker:      check.NewChecker(),
	}
}

func (p *Package) AddFile(file *tree.FileDef) {
	p.Files = append(p.Files, file)
}

func (p *Package) AddDependency(ip ImportPath) {
	p.Dependencies[ip] = struct{}{}
}
