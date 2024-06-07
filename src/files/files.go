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

func NewFinder(paths []string) Finder {
	return &finder{
		GOROOT: ReadGoEnv("GOROOT"),
		paths:  paths,
	}
}

type finder struct {
	GOROOT string
	paths  []string
}

func (f *finder) FindImport(ip ImportPath) (string, error) {
	path := ip.String()

	candidates := []string{
		filepath.Join(f.GOROOT, "src", path),
		filepath.Join(f.GOROOT, "src", "vendor", path),
	}
	for _, p := range f.paths {
		candidates = append(candidates, filepath.Join(p, path))
	}

	for _, candidate := range candidates {
		if _, err := os.Stat(candidate); err == nil {
			return candidate, nil
		}
	}

	return "", fmt.Errorf("import not found: %v", path)
}
