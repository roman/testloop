#!/bin/bash

function _runCabalSandboxTestLoop {
    cabal sandbox init &&
    cabal sandbox add-source ../../../testloop &&
    cabal install --enable-tests &&
    # cabal-dev install testloop --reinstall
    echo -e "\x1B[32mTestLoop Ready\x1B[0m"
    echo -e "Go to \x1B[33mtest/TestSuite.hs\x1B[0m, modify the file and see things running automatically"
    echo ""
    .cabal-sandbox/bin/testloop
    ./dist/build/testloop/testloop
}

function _runCabalDevTestLoop {
    cabal-dev add-source ../../../testloop &&
    cabal-dev configure --enable-test &&
    cabal-dev install --only-dependencies &&
    # cabal-dev install testloop --reinstall
    cabal-dev clean &&
    cabal-dev build &&
    echo -e "\x1B[32mTestLoop Ready\x1B[0m"
    echo -e "Go to \x1B[33mtest/TestSuite.hs\x1B[0m, modify the file and see things running automatically"
    echo ""
    ./dist/build/testloop/testloop
}

if [[ $1 = "cabal-sandbox" ]]; then
    command -v cabal && _runCabalSandboxTestLoop
else
    command -v cabal-dev && _runCabalDevTestLoop
fi
