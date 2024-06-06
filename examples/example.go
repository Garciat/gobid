package main

import "unsafe"

func example1(p unsafe.Pointer) uintptr {
	return (uintptr)(p)
}

func example2(p unsafe.Pointer) *int {
	return (*int)(p)
}

func twoints() (int, int) {
	return 1, 3
}

func run() int32 {
	// fmt.Println("Hello, World!")
	// os.Environ()
	return 0
}

// ==================================================================

func add[T int](x, y T) T {
	return x + y + 3
}

func first[T any](x, y T) T {
	return x
}

func Nil[T any]() T {
	var zero T
	return zero
}

func firstIndex[T any](x []T) T {
	return x[Nil()]
}

func cast[T U, U any](x T) U {
	return x
}

type Hello[T string | int] struct {
	Value T
}

type HelloInt struct {
	Value Hello[int]
}

type TwoHello[T string] struct {
	Value1 Hello[T]
	Value2 Hello[int]
}

type Vec[T any] struct{}

func MakeVec[T any]() Vec[T] {}

func Append[T any](v Vec[T], x T) {}

func ReadVec[T any](v Vec[T]) T {}

func Ptr[T any](x T) *T {
	return &x
}

func useVec() {
	v := MakeVec()
	Append(v, Nil())
	Append(v, Ptr(32))
}

func alloc[T any]() *T {
	return new(T)
}

func hello[T int]() []int {
	return make([]T)
}

func inferStructField() int {
	v := MakeVec()
	Append(v, Nil())
	return ReadVec(v)
}

type F = interface{ F() []int }

func callF[T F](t T) []int {
	return t.F()
}

func accessField[U any](t struct{ F U }) U {
	return t.F
}

type Pair[T, U any] struct {
	First  T
	Second U
}

func makePair[T, U any](x T, y U) Pair[T, U] {
	return Pair[T, U]{First: x, Second: y}
}

type TyEq[T, U any] interface {
	Apply(T) U
}

type TyEqImpl[T any] struct{}

func (_ TyEqImpl[T]) Apply(x T) T {
	return x
}

func Refl[T any]() TyEq[T, T] {
	return TyEqImpl[T]{}
}

type Greeter interface {
	Greet()
}

type Person struct{}

func (*Person) Greet() {}

func MakeGreeter() Greeter {
	return &Person{}
}

type Maker[T any] interface {
	Make() T
}

type IntMaker struct{}

func (IntMaker) Make() int { return 0 }

func UseIntMaker() Maker[int] {
	return &IntMaker{}
}

func PointerThing[T any, U *T | int](t T) U {
	var empty U
	return empty
}

type Receiver struct{}

func (*Receiver) Run() {}

type Runner interface {
	Run()
}

func PointerReceiverThing[T any, U interface {
	*T
	Runner
}](t T) {
	var u U = &t
	u.Run()
}

func UsePointerReceiverThing() {
	PointerReceiverThing(Receiver{})
}

type X struct{}

func (x *X) M() int { return 42 }

func CallX_M(x X) int {
	return x.M()
}

func CallHasM[T interface {
	*X
	M() R
}, R any](t T) R {
	return t.M()
}

func getValue(id *struct{ Value int }) int {
	return id.Value
}

func variadic(xs ...int) []int {
	for _, x := range xs {

	}
	return xs
}

func useVariadic() {
	variadic(1, 2, 3)
	variadic([]int{1, 2, 3}...)
}

func parse() (int, error) {
	return 0, nil
}

func useParse() bool {
	i, err := parse()
	return i == 0 && err == nil
}

func callParse() (int, error) {
	return parse()
}
