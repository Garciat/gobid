.PHONY: test
test:
	go run ./src/cmd/tests ./examples
	go run ./src/cmd/tests ./tests


.PHONY: main.wasm
main.wasm:
	GOOS=js GOARCH=wasm go build -o main.wasm github.com/garciat/gobid/cmd/main

.PHONY: gh-pages-dev
gh-pages-dev: main.wasm
	cp main.wasm gh-pages/src/build
	cd gh-pages && deno task serve
