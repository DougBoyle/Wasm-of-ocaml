#!/usr/bin/bash
cd $(dirname $BASH_SOURCE)

for file in ../js/*.js
do
  node --expose-gc js_timing.js $file
done