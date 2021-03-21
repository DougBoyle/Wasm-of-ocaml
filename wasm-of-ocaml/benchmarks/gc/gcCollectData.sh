#!/usr/bin/bash
cd $(dirname $BASH_SOURCE) # always run from benchmark directory for ease of use
for ITERS in 5 6 7 8 9 10
do
  (echo "let iters = $ITERS" && cat alltreestest.ml) > tmp.ml
  ../../main.byte tmp.ml
  node iterationsTiming.js tmp.wasm $ITERS
done

#for ITERS in $(seq 10 5 300)
#do
#  (echo "let iters = $ITERS" && cat malicious.ml) > tmp.ml
#  ../../main.byte tmp.ml
#  node iterationsTiming.js tmp.wasm $ITERS
#done

#for ITERS in $(seq 50 50 4000)
#do
#  (echo "let iters = $ITERS" && cat mergesort.ml) > tmp.ml
#  ../../main.byte tmp.ml
#  node iterationsTiming.js tmp.wasm $ITERS
#done