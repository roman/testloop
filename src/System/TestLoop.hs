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
import           Data.Maybe                       (catMaybes)
import           Data.Monoid                      (mconcat)
import           System.Directory                 (doesFileExist)
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

getTestMainFilePath :: HsSourcePaths -> MainModulePath -> IO (Either String FilePath)
getTestMainFilePath sourcePaths modulePath = do
    mainPaths <- mapM getPossiblePath sourcePaths
    case (catMaybes mainPaths) of
      [completeModulePath] -> return $ Right completeModulePath
      [] -> return . Left $ mconcat [ "Could not find `",
                                      modulePath,
                                      "' in ",
                                      show sourcePaths]
      multipleMatches -> return . Left $ mconcat [ "Multiple matches for test `Main' module"
                                                , "on source-paths: \n",
                                                show mainPaths ]

  where
    getPossiblePath sourcePath = do
      let completeModulePath = joinPath [sourcePath, modulePath]
      fileExists <- doesFileExist completeModulePath
      if fileExists
        then return $ Just completeModulePath
        else return Nothing

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
 result <- getTestMainFilePath sourcePaths moduleFile
 case result of
   Left e -> hPutStrLn stderr e
   Right fullModuleFilePath -> do
     putStrLn $ "Found test-suite main function on `" ++ fullModuleFilePath ++ "'"
     putStrLn $ "Listening files on source paths: " ++ (join ", " sourcePaths)
     _ <- forkIO $ startTestLoop "Main"
                                 fullModuleFilePath
                                 sourcePaths
     forever $ threadDelay 100
