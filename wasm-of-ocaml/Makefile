INCLUDES=-I src/ocaml/file_formats -I src/ocaml/lambda -I src/ocaml/driver -I src/ocaml/typing \
        -I src/ocaml/utils -I src/ocaml/parsing -I src/middle-end -I src/codegen \
        -I src/optimisations -I src/graph -I src
all:
	ocamlbuild -pkg wasm $(INCLUDES) main.byte
unittest:
	make all
	set -e
	for f in samples/*.ml ; do ./main.byte -d samples/out $$f -w -8-26 || { echo $$f ; exit 1 ; }  ; done
	node samples/test.js
rt:
	wat2wasm src/runtime/runtime.wast
	mv runtime.wasm src/runtime/runtime.wasm
	wat2wasm src/runtime/no_gc_runtime.wast
	mv no_gc_runtime.wasm src/runtime/no_gc_runtime.wasm
lintest:
	ocamlbuild -pkg ounit2 $(INCLUDES) -I unit_tests test_linast_utils.byte
analysistest:
	ocamlbuild -pkg ounit2 $(INCLUDES) -I unit_tests test_analysis.byte
constproptest:
	ocamlbuild -pkg ounit2 $(INCLUDES) -I unit_tests test_const_prop.byte
uncurrytest:
	ocamlbuild -pkg ounit2 $(INCLUDES) -I unit_tests test_uncurry.byte
lvatest:
	ocamlbuild -pkg ounit2 -pkg wasm $(INCLUDES) -I unit_tests test_lva.byte
graphtest:
	ocamlbuild -pkg ounit2 -pkg wasm $(INCLUDES) -I unit_tests test_graph.byte
testall:
	ocamlbuild -pkg ounit2 -pkg wasm $(INCLUDES) -I unit_tests run_all_tests.byte
	./run_all_tests.byte
cleanout:
	rm benchmarks/out/*.wasm
	rm benchmarks/js/*.js
	rm benchmarks/js/*.byte
	rm benchmarks/grain/out/*.wasm
	rm benchmarks/C/out/*.wasm
	rm benchmarks/gc/*.wasm
	rm benchmarks/Ocaml/*.cmo
	rm samples/out/*
interfaces:
	for f in benchmarks/Ocaml/*.mli ; do \
	  ocamlc $$f ; \
	  NAME=$$(basename $$f .mli) ; \
	  mv benchmarks/Ocaml/$${NAME}.cmi benchmarks/out/$${NAME}.cmi ; \
	done

