dirs = src/ocaml/file_formats,src/ocaml/lambda,src/ocaml/driver,src/ocaml/typing,src/ocaml/utils,src/ocaml/parsing,src
dirs2 = src/ocaml/file_formats,src/ocaml/lambda,src/ocaml/driver,src/ocaml/typing,src/ocaml/utils,src/ocaml/parsing,src/middle-end,src
all:
	ocamlbuild -pkg unix -Is $(dirs2) main.byte
ir:
	ocamlbuild -pkg unix -Is $(dirs)/middle-end linast.cmi
linear:
	ocamlbuild -pkg unix -Is $(dirs)/middle-end linearise.byte
