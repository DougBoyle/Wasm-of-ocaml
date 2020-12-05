#!/usr/bin/bash
cd $(dirname $BASH_SOURCE)
# Need to be careful using -O3, can delete code if it realises it isn't being used
# can also inline functions to realise they always return a constant
for file in *.c
do
  if grep -q "#include" $file
  then
    clang --target=wasm32-unknown-wasi --sysroot ../../../../wasi-libc/sysroot -nostartfiles -Wl,--no-entry -Wl,--export=main -Wl,--export=sbrk -o out/$(basename -- $file).wasm $file
  else
    clang --target=wasm32-unknown-wasi -nostdlib -nostartfiles -Wl,--no-entry -Wl,--export=main -o out/$(basename -- $file).wasm $file
  fi
done
for file in out/*.wasm
do
    node c_runner.js $file
done
