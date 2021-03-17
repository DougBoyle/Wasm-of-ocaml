#!/usr/bin/bash
cd $(dirname $BASH_SOURCE)

for file in ../C/out/*.wasm
do
  node c_timing.js $file
done