#!/bin/bash

_runTestLoop {
    cabal-dev add-source ../../../testloop
    cabal-dev clean
    cabal-dev configure --enable-tests
    cabal-dev build
    echo -e "\x1B[32mTestLoop Ready\x1B[0m"
    echo -e "Go to \x1B[33mtest/TestSuite.hs\x1B[0m, modify the file and see things running automatically"
    echo ""
    ./dist/build/testloop/testloop
}

command -v cabal-dev || _runTestLoop
