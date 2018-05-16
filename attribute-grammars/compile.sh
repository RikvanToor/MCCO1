#!/bin/bash

set -e # exit on error
pushd src
make
popd
cabal build

