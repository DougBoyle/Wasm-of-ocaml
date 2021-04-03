#!/usr/bin/bash
cd $(dirname $BASH_SOURCE)
for file in gc_*.c
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
#for file in out/*.wasm
#do
#    node ../runner.js $file
#done
