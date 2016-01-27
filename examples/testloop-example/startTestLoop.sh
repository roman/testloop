#!/bin/bash

function _runStackTestLoop {
    stack build
    echo -e "\x1B[32mTestLoop Ready\x1B[0m"
    echo -e "Go to \x1B[33mtest/TestSuite.hs\x1B[0m, modify the file and see things running automatically"
    echo ""
    stack exec testloop
}

command -v stack && _runStackTestLoop
