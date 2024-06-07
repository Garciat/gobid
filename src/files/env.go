package files

import (
	"os/exec"
	"strings"
)

func ReadGoEnv(name string) string {
	out, err := exec.Command("go", "env", name).Output()
	if err != nil {
		return ""
	}
	return strings.TrimSpace(string(out))
}
