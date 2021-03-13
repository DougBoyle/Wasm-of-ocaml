#!/usr/bin/bash
cd $(dirname $BASH_SOURCE)

NO_GC=1

for file in ../out/*.wasm
do
  if (( NO_GC ))
  then
    node no_gc_timing.js $file
  else
    node avg_timing.js $file
  fi
done