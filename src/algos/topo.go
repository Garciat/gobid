package algos

import (
	"slices"
)

func TopologicalSort[T any, K comparable](nodes map[K]T, edges func(T) map[K]struct{}) []T {
	inDegree := map[K]int{}
	for k := range nodes {
		inDegree[k] = 0
	}
	for _, node := range nodes {
		for dep := range edges(node) {
			inDegree[dep]++
		}
	}

	var queue []K
	for k, degree := range inDegree {
		if degree == 0 {
			queue = append(queue, k)
		}
	}

	var sorted []T
	for len(queue) > 0 {
		k := queue[0]
		queue = queue[1:]
		sorted = append(sorted, nodes[k])
		for dep := range edges(nodes[k]) {
			inDegree[dep]--
			if inDegree[dep] == 0 {
				queue = append(queue, dep)
			}
		}
	}

	//Check for cycles beforehand :shrug:
	//if len(sorted) != len(nodes) {
	//	panic("cycle in dependencies")
	//}

	slices.Reverse(sorted)

	return sorted
}
