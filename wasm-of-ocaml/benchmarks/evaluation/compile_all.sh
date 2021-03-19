#!/usr/bin/bash

NO_GC=1
# put options for optimisation here. Choices are:
# -Nopt-ir, -passes-ir, -Nopt-graph, -passes-graph, -Nopt-patterns
OPTS=(  )

# Shared tests are: alltrees_7, arith_75, composition, funcrec, mergesort_500, nbody_100
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
done

./time_all.sh