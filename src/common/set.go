package common

type Set[T comparable] map[T]struct{}

func NewSet[T comparable]() Set[T] {
	return make(Set[T])
}

func (s Set[T]) Add(v T) {
	s[v] = struct{}{}
}

func (s Set[T]) Contains(v T) bool {
	_, ok := s[v]
	return ok
}

func MergeSets[T comparable](sets ...Set[T]) Set[T] {
	result := NewSet[T]()
	for _, set := range sets {
		for v := range set {
			result.Add(v)
		}
	}
	return result
}
