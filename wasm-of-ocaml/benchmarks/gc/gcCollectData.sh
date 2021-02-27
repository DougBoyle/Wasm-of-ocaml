#!/usr/bin/bash
cd $(dirname $BASH_SOURCE) # always run from benchmark directory for ease of use
for ITERS in 5 6 7 8 9
do
  (echo "let iters = $ITERS" && cat alltreestest.ml) > tmp.ml
  ../../main.byte tmp.ml
  node iterationsTiming.js tmp.wasm $ITERS
done

#for ITERS in 10 20 30 40 50 60 70 80 90 100
#do
#  (echo "let iters = $ITERS" && cat malicious.ml) > tmp.ml
#  ../../main.byte tmp.ml
#  node iterationsTiming.js tmp.wasm $ITERS
#done