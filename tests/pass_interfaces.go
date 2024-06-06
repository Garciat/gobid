package tests

var _ interface{ M() } = (interface{ M() })(nil)

var _ interface{ M() } = (interface {
	M()
	N()
})(nil)
