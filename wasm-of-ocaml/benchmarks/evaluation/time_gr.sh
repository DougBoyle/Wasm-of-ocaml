#!/usr/bin/bash
cd $(dirname $BASH_SOURCE)

for file in ../grain/out/*.wasm
do
  node grain_timing.js $file 0
done

# switch to dev version of runtime for tracing information
cd $GRAIN_STDLIB/..
yarn runtime build:dev --silent > /dev/null 2>&1
cd - > /dev/null 2>&1
for file in ../grain/out/*.wasm
do
  node grain_timing.js $file 1
done

# switch back to default runtime
cd $GRAIN_STDLIB/..
yarn runtime build --silent > /dev/null 2>&1
cd - > /dev/null 2>&1
