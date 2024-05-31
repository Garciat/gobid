package files

import (
	"fmt"
	"os/exec"
	"strings"
)

func GetGOROOT() string {
	out, err := exec.Command("go", "env", "GOROOT").Output()
	if err != nil {
		panic(fmt.Errorf("failed to get GOROOT: %w", err))
	}
	return strings.TrimSpace(string(out))
}

func GetGOPATH() string {
	out, err := exec.Command("go", "env", "GOPATH").Output()
	if err != nil {
		panic(fmt.Errorf("failed to get GOPATH: %w", err))
	}
	return strings.TrimSpace(string(out))
}
