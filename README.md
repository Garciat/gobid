# A type checker for Golang with better inference

## Running tests

```sh
make test
```

## Running web demo

```sh
go run ./src/cmd/webapp
```

## Build GitHub Pages

```sh
GOOS=js GOARCH=wasm go build -o gh-pages/src/build/main.wasm github.com/garciat/gobid/cmd/main
cd gh-pages
deno task build
```
