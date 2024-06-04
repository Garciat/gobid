package common

type Set[T comparable] map[T]struct{}

func NewSet[T comparable](elems ...T) Set[T] {
	s := make(Set[T])
	s.AddAll(elems...)
	return s
}

func (s Set[T]) Add(v T) {
	s[v] = struct{}{}
}

func (s Set[T]) AddAll(elems ...T) {
	for _, v := range elems {
		s.Add(v)
	}
}

func (s Set[T]) Contains(v T) bool {
	_, ok := s[v]
	return ok
}

func (s Set[T]) Merge(t Set[T]) {
	for v := range t {
		s.Add(v)
	}
}

func (s Set[T]) Remove(v T) {
	delete(s, v)
}

func (s Set[T]) RemoveAll(other Set[T]) Set[T] {
	for k := range other {
		s.Remove(k)
	}
	return s
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

func MergeSets2[T comparable](a, b Set[T]) Set[T] {
	return MergeSets(a, b)
}
