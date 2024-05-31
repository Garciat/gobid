package gocompiler

// copied from https://github.com/golang/go/blob/5bf8c0cf09ee5c7e5a37ab90afcce154ab716a97/src/cmd/go/internal/cfg/cfg.go

import (
	"strings"
	"unicode"
)

// MatchFile returns false if the name contains a $GOOS or $GOARCH
// suffix which does not match the current system.
// The recognized name formats are:
//
//	name_$(GOOS).*
//	name_$(GOARCH).*
//	name_$(GOOS)_$(GOARCH).*
//	name_$(GOOS)_test.*
//	name_$(GOARCH)_test.*
//	name_$(GOOS)_$(GOARCH)_test.*
//
// Exceptions:
//
//	if GOOS=android, then files with GOOS=linux are also matched.
//	if GOOS=illumos, then files with GOOS=solaris are also matched.
//	if GOOS=ios, then files with GOOS=darwin are also matched.
//
// If tags["*"] is true, then MatchFile will consider all possible
// GOOS and GOARCH to be available and will consequently
// always return true.
func MatchFile(name string, tags map[string]bool) bool {
	if tags["*"] {
		return true
	}
	if dot := strings.Index(name, "."); dot != -1 {
		name = name[:dot]
	}

	// Before Go 1.4, a file called "linux.go" would be equivalent to having a
	// build tag "linux" in that file. For Go 1.4 and beyond, we require this
	// auto-tagging to apply only to files with a non-empty prefix, so
	// "foo_linux.go" is tagged but "linux.go" is not. This allows new operating
	// systems, such as android, to arrive without breaking existing code with
	// innocuous source code in "android.go". The easiest fix: cut everything
	// in the name before the initial _.
	i := strings.Index(name, "_")
	if i < 0 {
		return true
	}
	name = name[i:] // ignore everything before first _

	l := strings.Split(name, "_")
	if n := len(l); n > 0 && l[n-1] == "test" {
		l = l[:n-1]
	}
	n := len(l)
	if n >= 2 && KnownOS[l[n-2]] && KnownArch[l[n-1]] {
		return matchTag(l[n-2], tags, true) && matchTag(l[n-1], tags, true)
	}
	if n >= 1 && KnownOS[l[n-1]] {
		return matchTag(l[n-1], tags, true)
	}
	if n >= 1 && KnownArch[l[n-1]] {
		return matchTag(l[n-1], tags, true)
	}
	return true
}

// matchTag reports whether the tag name is valid and tags[name] is true.
// As a special case, if tags["*"] is true and name is not empty or ignore,
// then matchTag will return prefer instead of the actual answer,
// which allows the caller to pretend in that case that most tags are
// both true and false.
func matchTag(name string, tags map[string]bool, prefer bool) bool {
	// Tags must be letters, digits, underscores or dots.
	// Unlike in Go identifiers, all digits are fine (e.g., "386").
	for _, c := range name {
		if !unicode.IsLetter(c) && !unicode.IsDigit(c) && c != '_' && c != '.' {
			return false
		}
	}

	if tags["*"] && name != "" && name != "ignore" {
		// Special case for gathering all possible imports:
		// if we put * in the tags map then all tags
		// except "ignore" are considered both present and not
		// (so we return true no matter how 'want' is set).
		return prefer
	}

	if tags[name] {
		return true
	}

	switch name {
	case "linux":
		return tags["android"]
	case "solaris":
		return tags["illumos"]
	case "darwin":
		return tags["ios"]
	case "unix":
		panic("TODO")
		// return unixOS[GOOS]
	default:
		return false
	}
}

var KnownOS = map[string]bool{
	"aix":       true,
	"android":   true,
	"darwin":    true,
	"dragonfly": true,
	"freebsd":   true,
	"hurd":      true,
	"illumos":   true,
	"ios":       true,
	"js":        true,
	"linux":     true,
	"nacl":      true, // legacy; don't remove
	"netbsd":    true,
	"openbsd":   true,
	"plan9":     true,
	"solaris":   true,
	"wasip1":    true,
	"windows":   true,
	"zos":       true,
}

// unixOS is the set of GOOS values matched by the "unix" build tag.
// This is not used for filename matching.
// This is the same list as in go/build/syslist.go and cmd/dist/build.go.
var unixOS = map[string]bool{
	"aix":       true,
	"android":   true,
	"darwin":    true,
	"dragonfly": true,
	"freebsd":   true,
	"hurd":      true,
	"illumos":   true,
	"ios":       true,
	"linux":     true,
	"netbsd":    true,
	"openbsd":   true,
	"solaris":   true,
}

var KnownArch = map[string]bool{
	"386":         true,
	"amd64":       true,
	"amd64p32":    true, // legacy; don't remove
	"arm":         true,
	"armbe":       true,
	"arm64":       true,
	"arm64be":     true,
	"ppc64":       true,
	"ppc64le":     true,
	"mips":        true,
	"mipsle":      true,
	"mips64":      true,
	"mips64le":    true,
	"mips64p32":   true,
	"mips64p32le": true,
	"loong64":     true,
	"ppc":         true,
	"riscv":       true,
	"riscv64":     true,
	"s390":        true,
	"s390x":       true,
	"sparc":       true,
	"sparc64":     true,
	"wasm":        true,
}
