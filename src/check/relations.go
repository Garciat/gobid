package check

import (
	"fmt"
	"github.com/garciat/gobid/tree"
)

type Relation interface {
	_Relation()
}

type RelationBase struct{}

func (RelationBase) _Relation() {}

type RelationEq struct {
	RelationBase
	Left  tree.Type
	Right tree.Type
}

func (r RelationEq) String() string {
	return fmt.Sprintf("%v =eq %v", r.Left, r.Right)
}

type RelationSubtype struct {
	RelationBase
	Sub   tree.Type
	Super tree.Type
}

func (r RelationSubtype) String() string {
	return fmt.Sprintf("%v <: %v", r.Sub, r.Super)
}

type RelationSatisfies struct {
	RelationBase
	Type       tree.Type
	Constraint *tree.InterfaceType
}

func (r RelationSatisfies) String() string {
	return fmt.Sprintf("%v sat %v", r.Type, r.Constraint)
}
