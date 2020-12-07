#!/usr/bin/bash
cd $(dirname $BASH_SOURCE)
for file in *.ml
do
    NAME=$(basename $file .ml)
    ocamlfind ocamlc -o js/${NAME}.byte -linkpkg -package js_of_ocaml,js_of_ocaml-ppx $file
    js_of_ocaml js/${NAME}.byte
    rm ${NAME}.cmi
    rm ${NAME}.cmo
done
for file in js/*.js
do
  node --expose-gc js_runner.js $file
done
