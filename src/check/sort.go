package check

import (
	"github.com/garciat/gobid/algos"
	"github.com/garciat/gobid/common"
	"github.com/garciat/gobid/source"
)

func SortPackages(packages map[common.ImportPath]*source.Package) []*source.Package {
	return algos.TopologicalSort(packages, func(pkg *source.Package) common.Set[common.ImportPath] {
		return pkg.Dependencies
	})
}
