#!/usr/bin/bash
cd $(dirname $BASH_SOURCE) # always run from benchmark directory for ease of use
#for ITERS in 5 6 7 8 9 10
#do
#  (echo "let iters = $ITERS" && cat alltreestest.ml) > tmp.ml
#  ../../main.byte tmp.ml
#  node iterationsTiming.js tmp.wasm $ITERS
#done

#for ITERS in 10 20 30 40 50 60 70 80 90 100 110 120 130 140 150 200 300 500 750 1000 1200 1400 1600 1800 2000
#do
#  (echo "let iters = $ITERS" && cat malicious.ml) > tmp.ml
#  ../../main.byte tmp.ml
#  node iterationsTiming.js tmp.wasm $ITERS
#done

for ITERS in $(seq 50 50 4000)
do
  (echo "let iters = $ITERS" && cat mergesort.ml) > tmp.ml
  ../../main.byte tmp.ml
  node iterationsTiming.js tmp.wasm $ITERS
done