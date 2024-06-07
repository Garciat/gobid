package main

func Nil[T any]() T {
	var zero T
	return zero
}

func Ptr[T any](x T) *T {
	return &x
}

type Vec[T any] struct{
	// ...
}

func MakeVec[T any]() Vec[T] {
	return Vec[T]{}
}

func Append[T any](v Vec[T], x T) {}

// Example inspired by https://rustc-dev-guide.rust-lang.org/type-inference.html
func main() {
	v := MakeVec()
	// T is not provided to MakeVec; the checker instantiates a fresh type variable @T0
	// so we effectively have: v := MakeVec[@T0]()
	// making v a Vec[@T0]

	Append(v, Nil())
	// same here, for both of the generic calls: Append, Nil
	// this becomes: Append[@T2](v, Nil[@T1]())
	// but the arguments elicit these type relationships:
	// Append(*, _): Vec[@T2] = Vec[@T0]
	// Append(_, *): @T2 = @T1

	Append(v, Ptr(32))
	// resolving the generic calls:
	// Append[@T4](v, Ptr[@T3](32))
	// with the following type relationships:
	// Append(*, _): Vec[@T4] = Vec[@T0]
	// Append(_, *): @T4 = *@T3
	// Ptr(*): @T3 = int

	// finally, at the end of the function scope, unification takes place
	// the following type replacements are made:
	// @T0 = *int
	// @T1 = int
	// @T2 = *int
	// @T3 = int
	// @T4 = *int
}