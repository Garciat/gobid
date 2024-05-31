package algos

func FindCycle[T any, K comparable](nodes map[K]T, edges func(T) map[K]struct{}) (cycle []T) {
	visited := map[K]bool{}
	recStack := map[K]bool{}

	var dfs func(K)
	dfs = func(k K) {
		if recStack[k] {
			cycle = append(cycle, nodes[k])
			return
		}
		if visited[k] {
			return
		}

		visited[k] = true
		recStack[k] = true

		for dep := range edges(nodes[k]) {
			dfs(dep)
		}

		recStack[k] = false
	}

	for k := range nodes {
		dfs(k)
	}

	return
}
