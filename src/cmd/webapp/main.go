package main

import (
	"fmt"
	"github.com/garciat/gobid/check"
	"github.com/garciat/gobid/common"
	"github.com/garciat/gobid/compile"
	"golang.org/x/text/encoding/unicode"
	"html/template"
	"log"
	"net/http"
	"os"
	"strings"
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
	log.SetFlags(0)

	mux := http.NewServeMux()
	mux.HandleFunc("GET /{$}", indexHandler)
	mux.HandleFunc("POST /compile", compileHandler)

	port := getPort()
	addr := fmt.Sprintf("0.0.0.0:%s", port)

	log.Println("Listening on " + addr)
	log.Fatal(http.ListenAndServe(addr, logRequest(mux)))
}

func getPort() string {
	port, ok := os.LookupEnv("PORT")
	if !ok {
		return "8080"
	}
	return port
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
	err := r.ParseMultipartForm(500 * 1024)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	filename := r.FormValue("filename")
	switch {
	case filename == "":
		fallthrough
	case !strings.HasSuffix(filename, ".go"):
		fallthrough
	case strings.Contains(filename, "/"):
		http.Error(w, "invalid filename", http.StatusBadRequest)
		return
	default:
		// OK
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
			unit.AddSource(filename, []byte(code))
			unit.Compile()
			return 0
		})
	}
	compilerMux.Unlock()

	if err != nil {
		fmt.Fprintf(w, "ERROR: %v", err)
	}
}

func logRequest(handler http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		log.Printf("%s %s %s\n", r.RemoteAddr, r.Method, r.URL)
		handler.ServeHTTP(w, r)
	})
}
