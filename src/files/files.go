package files

import (
	"fmt"
	"os"
	"path/filepath"

	. "github.com/garciat/gobid/common"
)

type Finder interface {
	FindImport(ip ImportPath) (string, error)
}

func NewFinder() Finder {
	return &finder{
		GOROOT: GetGOROOT(),
	}
}

type finder struct {
	GOROOT string
}

func (f *finder) FindImport(ip ImportPath) (string, error) {
	path := ip.String()

	candidates := []string{
		filepath.Join(f.GOROOT, "src", path),
		filepath.Join(f.GOROOT, "src", "vendor", path),
	}

	for _, candidate := range candidates {
		if _, err := os.Stat(candidate); err == nil {
			return candidate, nil
		}
	}

	return "", fmt.Errorf("import not found: %v", path)
}
