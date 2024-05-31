package source

import (
	. "github.com/garciat/gobid/common"
)

type Package struct {
	ImportPath   ImportPath
	Dependencies map[ImportPath]struct{}
	Files        []*FileDef
}

func NewPackage(ip ImportPath) *Package {
	return &Package{
		ImportPath:   ip,
		Dependencies: make(map[ImportPath]struct{}),
		Files:        nil,
	}
}

func (p *Package) AddFile(file *FileDef) {
	p.Files = append(p.Files, file)
}

func (p *Package) AddDependency(ip ImportPath) {
	p.Dependencies[ip] = struct{}{}
}
