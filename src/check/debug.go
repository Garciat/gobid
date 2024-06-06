package check

import (
	"fmt"
)

const (
	DebugUnify   = false
	DebugGeneral = false
	DebugChecker = false
)

func UnifyPrintf(format string, args ...interface{}) {
	if DebugUnify {
		fmt.Printf(format, args...)
	}
}

func GeneralPrintf(format string, args ...interface{}) {
	if DebugGeneral {
		fmt.Printf(format, args...)
	}
}

func CheckerPrintf(format string, args ...interface{}) {
	if DebugChecker {
		fmt.Printf(format, args...)
	}
}
