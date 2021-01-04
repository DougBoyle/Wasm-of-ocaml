#!/usr/bin/bash
cd $(dirname $BASH_SOURCE) # always run from benchmark directory for ease of use
for file in *.ml
do
  ../main.byte -d out $file
done
for file in out/*.wasm
do
  node runner.js $file
done