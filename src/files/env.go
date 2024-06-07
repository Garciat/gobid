package files

import (
	"fmt"
	"os/exec"
	"strings"
)

func ReadGoEnv(name string) string {
	out, err := exec.Command("go", "env", name).Output()
	if err != nil {
		panic(fmt.Errorf("failed to read go env: %w", err))
	}
	return strings.TrimSpace(string(out))
}
