package main

import (
	"github.com/garciat/gobid/check"
	"github.com/garciat/gobid/common"
	"github.com/garciat/gobid/compile"
	"golang.org/x/text/encoding/unicode"
	"html/template"
	"log"
	"net/http"
	"os"
	"sync"
)

var (
	defaultContent string
	compilerMux    sync.Mutex
)

func init() {
	data, err := os.ReadFile("cmd/webapp/resources/default.go")
	if err != nil {
		log.Fatal(err)
	}
	data, err = unicode.UTF8.NewDecoder().Bytes(data)
	if err != nil {
		log.Fatal(err)
	}
	defaultContent = string(data)
}

func main() {
	mux := http.NewServeMux()
	mux.HandleFunc("GET /", indexHandler)
	mux.HandleFunc("POST /compile", compileHandler)
	log.Fatal(http.ListenAndServe(":8080", mux))
}

func indexHandler(w http.ResponseWriter, r *http.Request) {
	type Page struct {
		DefaultContent string
	}

	t, _ := template.ParseFiles("cmd/webapp/resources/index.html")
	err := t.Execute(w, Page{DefaultContent: defaultContent})
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

func compileHandler(w http.ResponseWriter, r *http.Request) {
	err := r.ParseMultipartForm(1 << 20)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	code := r.FormValue("code")

	w.Header().Set("Content-Type", "text/plain")

	// Unfortunately, logging is not thread-safe, so we need to lock the compiler (':
	compilerMux.Lock()
	{
		*check.DebugAll = true
		check.DebugWriter = w

		_, err, _ = common.Try(func() int {
			unit := compile.NewCompilationUnit("main")
			unit.AddSource("main.go", []byte(code))
			unit.Compile()
			return 0
		})
	}
	compilerMux.Unlock()

	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}
