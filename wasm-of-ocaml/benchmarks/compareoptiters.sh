#!/usr/bin/bash
# run all benchmarks with all optimisations, varying the number of optimisation passes
cd $(dirname $BASH_SOURCE)
ITERS=5
{
  for n in $(seq 0 5)
  do
    >&2 echo "Test " $n
    echo $n
    for file in *.ml
    do
      NAME=$(basename $file .ml)
      ../main.byte -d out $file -passes-ir $n -passes-graph $n
    done
    for file in out/*.wasm
    do
      for i in $(seq 1 $ITERS)
      do
        node runner.js $file
      done
    done
  done
} > "results/$(date +"passes_%m_%d_%H_%M").txt"
