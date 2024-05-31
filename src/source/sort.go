package source

import (
	"github.com/garciat/gobid/algos"
	"github.com/garciat/gobid/common"
)

func TopologicalSort(packages map[common.ImportPath]*Package) []*Package {
	return algos.TopologicalSort(packages, func(pkg *Package) map[common.ImportPath]struct{} {
		return pkg.Dependencies
	})
}
