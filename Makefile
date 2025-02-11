.PHONY: test
test:
	go run ./src/cmd/tests ./examples
	go run ./src/cmd/tests ./tests

.PHONY: build/main.wasm
build/main.wasm:
	GOOS=wasip1 GOARCH=wasm go build -o build/main.wasm github.com/garciat/gobid/cmd/main

.PHONY: gh-pages-dev
gh-pages-dev: build/main.wasm
	cp build/main.wasm gh-pages/src/build
