#!/usr/bin/bash
# Shared tests are: alltrees_7, arith_75, composition, funcrec, mergesort_500, nbody_100
cd $(dirname $BASH_SOURCE)
ITERS=5
{
  >&2 echo "Starting OCaml"
  echo OCaml
#  for file in *.ml
  for file in {alltrees_7,arith_75,composition,funcrec,mergesort_500,nbody_100}.ml
  do
	  NAME=$(basename $file .ml)
	  ../main.byte -I out -d out $file
	  ocamlfind ocamlc -I out -o js/${NAME}.byte -linkpkg -package js_of_ocaml,js_of_ocaml-ppx $file
	  js_of_ocaml js/${NAME}.byte
	  rm ${NAME}.cmo
  done
  for file in out/*.wasm
  do
	  for i in $(seq 1 $ITERS)
	  do
	    node runner.js $file
	  done
  done
  >&2 echo "Starting JS"
  echo
  echo JS
  for file in js/*.js
  do
	  for i in $(seq 1 $ITERS)
	  do
	    node --expose-gc runner.js $file
	  done
  done
  cd grain
  >&2 echo "Starting Grain"
  echo
  echo Grain
  for file in *.gr
  do
	  grainc $file -o out/${file}.wasm
  done
  for file in out/*.gr.wasm
  do
	  for i in $(seq 1 $ITERS)
	  do
	    node ../runner.js $file
	  done
  done
  cd $GRAIN_STDLIB/..
  >&2 yarn runtime build:dev --silent
  >&2 cd -
  for file in out/*.gr.wasm
  do # memory measurement deterministic for Grain
    node wasmRunGrainMem.js $file
  done
  cd $GRAIN_STDLIB/..
  >&2 yarn runtime build --silent
  >&2 cd -
  cd ../C
  >&2 echo "Starting C"
  echo
  echo C
#  for file in *.c
  for file in {alltrees_7,arith_75,composition,funcrec,mergesort_500,nbody_100}.c
  do
	  NAME=$(basename $file .c)
	  if grep -q "#include" $file
	  then
	    clang --target=wasm32-unknown-wasi --sysroot ../../../../wasi-libc/sysroot \
		  -nostartfiles -Wl,--no-entry -Wl,--export=main -Wl,--export=sbrk -o out/$NAME.c.wasm $file
	  else
	    clang --target=wasm32-unknown-wasi -nostdlib -nostartfiles \
		  -Wl,--no-entry -Wl,--export=main -o out/$NAME.c.wasm $file
	  fi
  done
  for file in out/*.wasm
  do
	  for i in $(seq 1 $ITERS)
	  do
	    node ../runner.js $file
	  done
  done
} > "results/$(date +"%m_%d_%H_%M").txt"
