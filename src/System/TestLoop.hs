{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: TestLoop.Main
-- Copyright: 2013 Roman Gonzalez
-- License: MIT
--
-- Maintainer: romanandreg@gmail.com
-- Portability: unix
--
module System.TestLoop (
  -- * Main function
  setupTestLoop
  ) where

--------------------

import           Control.Concurrent               (forkIO, threadDelay)
import           Control.Monad                    (forM_, forever)
import           Data.Monoid                      (mconcat)
import           System.FilePath                  (joinPath)
import           System.IO                        (hPutStrLn, stderr)

--------------------

import qualified Filesystem.Path.CurrentOS        as FS

--------------------

import           System.FSNotify                  (withManager)
import           System.FSNotify.Devel            (treeExtExists)
--------------------

import           System.TestLoop.Internal.Cabal
import           System.TestLoop.Internal.Types
import           System.TestLoop.Internal.Watcher
import           System.TestLoop.Util

--------------------------------------------------------------------------------

startTestLoop :: MainModuleName -> MainModulePath -> HsSourcePaths -> IO ()
startTestLoop moduleName modulePath paths =
   withManager $ \manager -> do
     forM_ paths $ \path -> do
       treeExtExists manager
                     (FS.decodeString path)
                     "hs"
                     (reloadTestSuite moduleName modulePath paths)
     forever $ threadDelay 100

--------------------------------------------------------------------------------

-- | Parses your project's cabal file to find possible test-suites you
--   may have on your project, then it will start a file modification
--   tracking and once a file is changed it will run the testsuite
--   automatically.  in the test-suite's hs-source-dirs setting.
--
-- Use this function as the main of you testloop executable.
-- e.g
--
-- > module Main where
-- >
-- > import System.TestLoop
-- >
-- > main :: IO ()
-- > main = setupTestLoop
--
setupTestLoop :: IO ()
setupTestLoop = do
 (testsuite, moduleFile, sourcePaths) <- parseCabalFile
 if not ("test" `elem` sourcePaths)
    then hPutStrLn stderr (mconcat [ "You must have a `test` folder in "
                                   , "your cabal's test-suite hs-source-paths"])
    else do
      putStrLn $ "Test Loop starting on test-suite '" ++ testsuite ++ "'"
      putStrLn $ "Listening files on source paths: " ++ (join ", " sourcePaths)
      _ <- forkIO $ startTestLoop "Main"
                                  (joinPath ["test", moduleFile])
                                  sourcePaths
      forever $ threadDelay 100
