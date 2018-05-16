#!/usr/bin/bash

sh ./compile.sh 1>&2 >> /dev/null

for testFile in $(ls examples/*.test); do
  echo "running for test: $testFile"
  cabal run $testFile
done
