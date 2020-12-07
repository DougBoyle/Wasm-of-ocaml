#!/usr/bin/bash
cd $(dirname $BASH_SOURCE)
{
  echo OCaml
    for file in *.ml
    do
	NAME=$(basename $file .ml)
	../main.byte -d out $file
	ocamlfind ocamlc -o js/${NAME}.byte -linkpkg -package js_of_ocaml,js_of_ocaml-ppx $file
	js_of_ocaml js/${NAME}.byte
	rm ${NAME}.cmi
	rm ${NAME}.cmo
    done
    for file in out/*.wasm
    do
	for i in {1..10}
	do
	    node ocaml_runner.js $file
	done
    done
    echo
    echo JS
    for file in js/*.js
    do
	for i in {1..10}
	do
	    node --expose-gc js_runner.js $file
	done
    done
    cd grain
    echo
    echo Grain
    for file in *.gr
    do
	grainc $file -o out/${file}.wasm
    done
    for file in out/*.gr.wasm
    do
	for i in {1..10}
	do
	    node wasmRunGrain.js $file
	done
    done
    cd ../C
    echo
    echo C
    for file in *.c
    do
	NAME=$(basename $file .c)
	if grep -q "#include" $file
	then
	    clang --target=wasm32-unknown-wasi --sysroot ../../../../wasi-libc/sysroot \
		  -nostartfiles -Wl,--no-entry -Wl,--export=main -Wl,--export=sbrk -o out/$NAME.wasm $file
	else
	    clang --target=wasm32-unknown-wasi -nostdlib -nostartfiles \
		  -Wl,--no-entry -Wl,--export=main -o out/$NAME.wasm $file
	fi
    done
    for file in out/*.wasm
    do
	for i in {1..10}
	do
	    node c_runner.js $file
	done
    done    
} > "results/$(date +"%m_%d_%I_%M_%p").txt"
