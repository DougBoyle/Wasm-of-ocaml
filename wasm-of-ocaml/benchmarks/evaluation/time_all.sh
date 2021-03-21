#!/usr/bin/bash
cd $(dirname $BASH_SOURCE)

NO_GC=0

for file in ../out/*.wasm
#for file in ../out/{alltrees_7,alltrees_9,arith_75,arith_1000,composition,funcrec,mergesort_1000,mergesort_3000,nbody_100,nbody_1000}.wasm
#for file in ../out/splice*.wasm
do
  if (( NO_GC ))
  then
    node no_gc_timing.js $file
  else
    node avg_timing.js $file
  fi
done