dirs = src/ocaml/file_formats,src/ocaml/lambda,src/ocaml/driver,src/ocaml/typing,src/ocaml/utils,src/ocaml/parsing,src/middle-end,src/wasm,src
all:
	ocamlbuild -Is $(dirs) main.byte
ir:
	ocamlbuild -Is $(dirs) linast.cmi
linear:
	ocamlbuild -Is $(dirs) linearise.byte
wasm:
	ocamlbuild -pkg wasm -Is $(dirs) compilebinds.byte
