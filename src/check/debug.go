package check

import (
	"flag"
	"fmt"
	"io"
	"os"
)

var (
	DebugAll     = flag.Bool("debug", true, "debug all")
	DebugUnify   = flag.Bool("debug-unify", false, "debug unify")
	DebugGeneral = flag.Bool("debug-general", false, "debug general")
	DebugChecker = flag.Bool("debug-checker", false, "debug checker")

	DebugWriter io.Writer = os.Stdout
)

func init() {
	flag.Parse()
}

func UnifyPrintf(format string, args ...interface{}) {
	if *DebugAll || *DebugUnify {
		_, err := fmt.Fprintf(DebugWriter, format, args...)
		if err != nil {
			panic(err)
		}
	}
}

func GeneralPrintf(format string, args ...interface{}) {
	if *DebugAll || *DebugGeneral {
		_, err := fmt.Fprintf(DebugWriter, format, args...)
		if err != nil {
			panic(err)
		}
	}
}

func CheckerPrintf(format string, args ...interface{}) {
	if *DebugAll || *DebugChecker {
		_, err := fmt.Fprintf(DebugWriter, format, args...)
		if err != nil {
			panic(err)
		}
	}
}
