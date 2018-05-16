#!/usr/bin/bash

echo -n "compiling... "
sh ./compile.sh 1>&2 >> /dev/null
echo "done."

for testFile in $(ls examples/*.test); do
  echo ""
  echo "--------------------------------------------------------------------------------"
  echo "                         running for test: $testFile"
  echo "--------------------------------------------------------------------------------"
  echo ""
  cabal run $testFile

  echo ""
  echo "Notes from $testFile header:"
  cat $testFile | head -n 3
done
