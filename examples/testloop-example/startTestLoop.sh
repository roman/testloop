#!/bin/bash

function _runTestLoop {
    cabal-dev add-source ../../../testloop
    cabal-dev configure --enable-test
    cabal-dev install --only-dependencies
    # cabal-dev install testloop --reinstall
    cabal-dev clean
    cabal-dev build
    echo -e "\x1B[32mTestLoop Ready\x1B[0m"
    echo -e "Go to \x1B[33mtest/TestSuite.hs\x1B[0m, modify the file and see things running automatically"
    echo ""
    ./dist/build/testloop/testloop
}

command -v cabal-dev && _runTestLoop
