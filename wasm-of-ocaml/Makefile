dirs = src/ocaml/file_formats,src/ocaml/lambda,src/ocaml/driver,src/ocaml/typing,src/ocaml/utils,src/ocaml/parsing,src/middle-end,src/wasm,src
all:
	ocamlbuild -pkg wasm -Is $(dirs) main.byte
ir:
	ocamlbuild -Is $(dirs) linast.cmi
linear:
	ocamlbuild -Is $(dirs) linearise.byte
wasm:
	ocamlbuild -pkg wasm -Is $(dirs) compilebinds.byte
unittest:
	make all
	node unit_tests/test.js
rt:
	wat2wasm src/runtime.wast
	mv runtime.wasm samples/runtime.wasm
cleanout:
	rm benchmarks/out/*.wasm
	rm benchmarks/js/*.js
	rm benchmarks/js/*.byte
	rm benchmarks/grain/out/*.wasm
	rm benchmarks/C/out/*.wasm
