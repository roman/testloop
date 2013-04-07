{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: TestLoop.Main
-- Copyright: 2013 Roman Gonzalez
-- License: MIT
--
-- Maintainer: romanandreg@gmail.com
-- Portability: unix
--
module TestLoop.Main (setupTestLoop) where

--------------------

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Monad             (forM_, forever)
import           Data.Monoid               (mconcat)
import           System.FilePath           (joinPath)
import           System.IO                 (hPutStrLn, stderr)

--------------------

import qualified Filesystem.Path.CurrentOS as FS

--------------------

import           System.FSNotify           (withManager)
import           System.FSNotify.Devel     (treeExtExists)
--------------------

import           TestLoop.Internal.Cabal
import           TestLoop.Internal.Types
import           TestLoop.Internal.Watcher
import           TestLoop.Util

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

-- | Use this function as the main of you testloop executable.
--
-- Parses your project's cabal file to figure out a test-suite
-- executable, then it will wait for modifications on files contained
-- in the test-suite's hs-source-dirs setting.

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
