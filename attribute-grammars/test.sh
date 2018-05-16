#!/usr/bin/bash

sh ./compile.sh

for testFile in $(ls examples/*.test); do
  echo "running for test: $testFile"
  cabal run $testFile
done
