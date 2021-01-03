#!/usr/bin/bash
# run all benchmarks with default passes (5) and original vs optimised pattern matching compilation
cd $(dirname $BASH_SOURCE)
ITERS=5
{
  >&2 echo "Unoptimised"
  echo Off
  for file in *.ml
  do
	  NAME=$(basename $file .ml)
	  ../main.byte -d out $file -Nopt-patterns
  done
  for file in out/*.wasm
  do
	  for i in $(seq 1 $ITERS)
	  do
	    node runner.js $file
	  done
  done

  >&2 echo "Optimised"
  echo On
  for file in *.ml
  do
	  NAME=$(basename $file .ml)
	  ../main.byte -d out $file
  done
  for file in out/*.wasm
  do
	  for i in $(seq 1 $ITERS)
	  do
	    node runner.js $file
	  done
  done
} > "results/$(date +"pats_%m_%d_%H_%M").txt"
