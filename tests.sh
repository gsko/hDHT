#!/bin/sh
cabal configure --enable-tests
cabal build
./dist/build/reference/reference
