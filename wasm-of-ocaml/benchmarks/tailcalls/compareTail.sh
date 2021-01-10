#!/usr/bin/bash
# run all benchmarks with different sets of optimisations enabled
cd $(dirname $BASH_SOURCE)
{
  for file in *.ml
  do
	  NAME=$(basename $file .ml)
	  ../../main.byte -d out $file
  done
  for file in out/*.wasm
  do
	  node ../runner.js $file
	  wasm -e '(input "../../samples/runtime.wasm") (register "ocamlRuntime") (input "'"$file"'") (invoke "OCAML$MAIN")'
  done
} > "new.txt"
