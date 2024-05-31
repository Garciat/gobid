package source

import (
	"github.com/garciat/gobid/common"
	"slices"
)

func TopologicalSort(packages map[common.ImportPath]*Package) []*Package {
	inDegree := map[common.ImportPath]int{}
	for _, pkg := range packages {
		inDegree[pkg.ImportPath] = 0
	}
	for _, pkg := range packages {
		for dep := range pkg.Dependencies {
			inDegree[dep]++
		}
	}

	var queue []common.ImportPath
	for ip, degree := range inDegree {
		if degree == 0 {
			queue = append(queue, ip)
		}
	}

	var sorted []*Package
	for len(queue) > 0 {
		ip := queue[0]
		queue = queue[1:]
		sorted = append(sorted, packages[ip])
		for dep := range packages[ip].Dependencies {
			inDegree[dep]--
			if inDegree[dep] == 0 {
				queue = append(queue, dep)
			}
		}
	}

	if len(sorted) != len(packages) {
		panic("cycle in package dependencies")
	}

	slices.Reverse(sorted)

	return sorted
}
