package common

import "fmt"

type Map[K comparable, V any] map[K]V

func NewMap[K comparable, V any]() Map[K, V] {
	return make(Map[K, V])
}

func (m Map[K, V]) Add(k K, v V) {
	m[k] = v
}

func (m Map[K, V]) Contains(k K) bool {
	_, ok := m[k]
	return ok
}

func (m Map[K, V]) Remove(k K) {
	delete(m, k)
}

func (m Map[K, V]) Merge(other Map[K, V]) {
	for k, v := range other {
		m.Add(k, v)
	}
}

func (m Map[K, V]) MergeFunc(other Map[K, V], merge func(V, V) V) {
	for k, v := range other {
		if m.Contains(k) {
			m.Add(k, merge(m[k], v))
		} else {
			m.Add(k, v)
		}
	}
}

func (m Map[K, V]) MergeStrict(other Map[K, V]) error {
	for k, v := range other {
		if m.Contains(k) {
			return fmt.Errorf("key %v already exists", k)
		}
		m.Add(k, v)
	}
	return nil
}

func (m Map[K, V]) Keys() Set[K] {
	result := NewSet[K]()
	for k := range m {
		result.Add(k)
	}
	return result
}

func MergeMaps[K comparable, V any](maps ...Map[K, V]) Map[K, V] {
	result := NewMap[K, V]()
	for _, m := range maps {
		result.Merge(m)
	}
	return result
}
