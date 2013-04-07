# TestLoop

TestLoop is a library that provides an automated execution and
reloading of a cabal's test-suites whenever a file is modified on the
hs-source-dirs

## Usage

Once you have a test suite using a haskell test library (hspec, HUnit,
test-framework, etc), write the following in your project's cabal file:

```cabal
test-suite tests
  type: exitcode-stdio-1.0
  hs-source-dirs: src, test
  main-is: TestSuite.hs
  -- configuration for your testsuite here

executable testloop
  main-is: TestLoop.hs
  -- all your project sources (+tests) directores
  hs-source-dirs: src, test
  build-depends:
    base             == 4.6.*,
    -- your project + test dependencies
    testloop         == 0.1.*
```

And in the `test/TestLoop.hs` file:

```haskell
import TestLoop.Main (setupTestLoop)

main :: IO ()
main = setupTestLoop
```

Install and run your testloop, as soon as you edit your source files
it will automatically compile and run your tests, instant feedback
FTW.