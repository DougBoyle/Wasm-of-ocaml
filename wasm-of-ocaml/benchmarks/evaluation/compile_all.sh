#!/usr/bin/bash

NO_GC=0
# put options for optimisation here. Choices are:
# -Nopt-ir, -passes-ir, -Nopt-graph, -passes-graph, -Nopt-patterns
OPTS=( )

cd $(dirname $BASH_SOURCE)
for file in ../Ocaml/*.ml
do
  # avoid compiling specific tests
  if [[ $file =~ splice ]] ; then
    continue;
  fi
  if (( NO_GC ))
  then
    ../../main.byte $file -d ../out -I ../out -No-gc "${OPTS[@]}"
  else
    ../../main.byte $file -d ../out -I ../out "${OPTS[@]}"
  fi
  NAME=$(basename $file .ml)
  ocamlfind ocamlc -o ../js/${NAME}.byte -I ../out -linkpkg -package js_of_ocaml,js_of_ocaml-ppx $file
  js_of_ocaml ../js/${NAME}.byte
  rm ../Ocaml/${NAME}.cmo
done

./time_all.sh