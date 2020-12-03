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
	for f in unit_tests/*.ml; do ./main.byte $$f -d unit_tests/out || exit; done
	node unit_tests/test.js
