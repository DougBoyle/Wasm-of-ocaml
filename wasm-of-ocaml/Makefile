INCLUDES=-I src/ocaml/file_formats -I src/ocaml/lambda -I src/ocaml/driver -I src/ocaml/typing \
        -I src/ocaml/utils -I src/ocaml/parsing -I src/middle-end -I src/wasm \
        -I src/wasmopt -I src/graph -I src
all:
	ocamlbuild -pkg wasm $(INCLUDES) main.byte
ir:
	ocamlbuild $(INCLUDES) linast.cmi
graph:
	ocamlbuild -pkg wasm $(INCLUDES) graph.byte
linear:
	ocamlbuild $(INCLUDES) linearise.byte
wasm:
	ocamlbuild -pkg wasm $(INCLUDES) compilebinds.byte
matrix:
	ocamlbuild $(INCLUDES) compileMatchOpt.byte
unittest:
	make all
	for f in unit_tests/*.ml ; do ./main.byte -d unit_tests/out $$f || exit 1 ; done
	node unit_tests/test.js
rt:
	wat2wasm src/runtime.wast
	mv runtime.wasm samples/runtime.wasm
lintest:
	ocamlbuild -pkg ounit2 $(INCLUDES) -I ounit test_linast_utils.byte
cleanout:
	rm benchmarks/out/*.wasm
	rm benchmarks/js/*.js
	rm benchmarks/js/*.byte
	rm benchmarks/grain/out/*.wasm
	rm benchmarks/C/out/*.wasm
