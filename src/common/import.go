package common

import (
	"strings"
)

type ImportPath string

func (p ImportPath) PackageName() string {
	parts := strings.Split(p.String(), "/")
	return parts[len(parts)-1]
}

func (p ImportPath) String() string {
	return string(p)
}
