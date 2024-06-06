package tests

var _ interface{ M() } = (interface {
	N()
})(nil)
