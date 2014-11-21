#!/bin/bash

function _runCabalSandboxTestLoop {
    [[ -d ../../.cabal-sandbox ]] || ( cd ../.. && cabal sandbox init && cd -; )
    cabal sandbox init --sandbox=../../.cabal-sandbox &&
    cabal sandbox add-source ../../../testloop &&
    cabal install --only-dependencies --enable-tests &&
    cabal configure --enable-tests &&
    cabal build
    echo -e "\x1B[32mTestLoop Ready\x1B[0m"
    echo -e "Go to \x1B[33mtest/TestSuite.hs\x1B[0m, modify the file and see things running automatically"
    echo ""
    ./dist/build/testloop/testloop
}

command -v cabal && _runCabalSandboxTestLoop
