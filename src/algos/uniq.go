package algos

func Uniq[T comparable](s []T) []T {
	seen := map[T]struct{}{}
	uniq := make([]T, 0, len(s))
	for _, v := range s {
		if _, ok := seen[v]; !ok {
			seen[v] = struct{}{}
			uniq = append(uniq, v)
		}
	}
	return uniq
}
