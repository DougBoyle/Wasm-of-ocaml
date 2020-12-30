#!/usr/bin/bash
cd $(dirname $BASH_SOURCE)
ITERS=5
{
  >&2 echo "Starting no optimisations"
  echo None
  for file in *.ml
  do
	  NAME=$(basename $file .ml)
	  ../main.byte -d out $file -Nopt-ir -Nopt-graph
  done
  for file in out/*.wasm
  do
	  for i in $(seq 1 $ITERS)
	  do
	    node runner.js $file
	  done
  done

  >&2 echo "Starting IR optimisations"
  echo Ir
  for file in *.ml
  do
	  NAME=$(basename $file .ml)
	  ../main.byte -d out $file -Nopt-graph
  done
  for file in out/*.wasm
  do
	  for i in $(seq 1 $ITERS)
	  do
	    node runner.js $file
	  done
  done

 >&2 echo "Starting Graph optimisations"
  echo Graph
  for file in *.ml
  do
	  NAME=$(basename $file .ml)
	  ../main.byte -d out $file -Nopt-ir
  done
  for file in out/*.wasm
  do
	  for i in $(seq 1 $ITERS)
	  do
	    node runner.js $file
	  done
  done

  >&2 echo "Starting all optimisations"
  echo All
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
} > "results/$(date +"opts_%m_%d_%H_%M").txt"
