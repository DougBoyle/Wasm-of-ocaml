#!/usr/bin/bash
cd $(dirname $BASH_SOURCE)
for file in *.gr
do
    grainc $file -o out/${file}.wasm
done
#for file in out/*.gr.wasm
#do
#  node ../runner.js $file
#done