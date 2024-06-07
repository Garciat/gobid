package check

import (
	"flag"
	"fmt"
)

var (
	DebugAll     = flag.Bool("debug", false, "debug all")
	DebugUnify   = flag.Bool("debug-unify", false, "debug unify")
	DebugGeneral = flag.Bool("debug-general", false, "debug general")
	DebugChecker = flag.Bool("debug-checker", false, "debug checker")
)

func init() {
	flag.Parse()
}

func UnifyPrintf(format string, args ...interface{}) {
	if *DebugAll || *DebugUnify {
		fmt.Printf(format, args...)
	}
}

func GeneralPrintf(format string, args ...interface{}) {
	if *DebugAll || *DebugGeneral {
		fmt.Printf(format, args...)
	}
}

func CheckerPrintf(format string, args ...interface{}) {
	if *DebugAll || *DebugChecker {
		fmt.Printf(format, args...)
	}
}
