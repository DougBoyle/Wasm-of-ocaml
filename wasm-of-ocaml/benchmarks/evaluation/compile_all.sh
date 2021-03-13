#!/usr/bin/bash
# Shared tests are: alltrees_7, arith_75, composition, funcrec, mergesort_500, nbody_100
NO_GC=0

cd $(dirname $BASH_SOURCE)
for file in ../Ocaml/*.ml
do
  if (( NO_GC ))
  then
    ../../main.byte $file -d ../out -I ../out -No-gc
  else
    ../../main.byte $file -d ../out -I ../out
  fi
done