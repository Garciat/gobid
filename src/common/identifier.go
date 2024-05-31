package common

var IgnoreIdent = Identifier{Value: "_"}

type Identifier struct {
	Value string
}

func (i Identifier) String() string {
	return i.Value
}

func NewIdentifier(name string) Identifier {
	return Identifier{name}
}
