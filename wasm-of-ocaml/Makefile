INCLUDES=-I src/ocaml/file_formats -I src/ocaml/lambda -I src/ocaml/driver -I src/ocaml/typing \
        -I src/ocaml/utils -I src/ocaml/parsing -I src/middle-end -I src/codegen \
        -I src/optimisations -I src/graph -I src
all:
	ocamlbuild -pkg wasm $(INCLUDES) main.byte
unittest:
	make all
	set -e
	for f in unit_tests/*.ml ; do ./main.byte -d unit_tests/out $$f || { echo $$f ; exit 1 ; }  ; done
	node unit_tests/test.js
rt:
	wat2wasm src/runtime/runtime.wast
	mv runtime.wasm src/runtime/runtime.wasm
lintest:
	ocamlbuild -pkg ounit2 $(INCLUDES) -I ounit test_linast_utils.byte
analysistest:
	ocamlbuild -pkg ounit2 $(INCLUDES) -I ounit test_analysis.byte
constproptest:
	ocamlbuild -pkg ounit2 $(INCLUDES) -I ounit test_const_prop.byte
graphtest:
	ocamlbuild -pkg ounit2 -pkg wasm $(INCLUDES) -I ounit test_graph.byte
testall:
	ocamlbuild -pkg ounit2 -pkg wasm $(INCLUDES) -I ounit run_all_tests.byte
	./run_all_tests.byte
cleanout:
	rm benchmarks/out/*.wasm
	rm benchmarks/js/*.js
	rm benchmarks/js/*.byte
	rm benchmarks/grain/out/*.wasm
	rm benchmarks/C/out/*.wasm
