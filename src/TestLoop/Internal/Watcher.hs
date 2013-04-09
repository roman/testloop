module TestLoop.Internal.Watcher (reloadTestSuite) where

--------------------

import           Control.Monad.Trans                 (MonadIO (..))
import           Data.List                           (isPrefixOf)
import           Data.Monoid                         (mconcat)
import qualified Filesystem.Path                     as FS
import qualified Filesystem.Path.CurrentOS           as FS

--------------------

import           Language.Haskell.Interpreter        (as, interpret,
                                                      loadModules, setImportsQ,
                                                      setTopLevelModules)
import           Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)

--------------------

import           Data.Time.LocalTime                 (getZonedTime)

--------------------

import           System.Directory                    (doesDirectoryExist,
                                                      getCurrentDirectory,
                                                      getDirectoryContents)
import           System.FilePath                     (joinPath)

--------------------

import           TestLoop.Internal.Types
import           TestLoop.Util

--------------------------------------------------------------------------------

getPackageDatabaseFile :: IO (Maybe FilePath)
getPackageDatabaseFile = do
  cabalDevExists <- doesDirectoryExist "cabal-dev"
  if (not cabalDevExists)
     then return Nothing
     else do
       dir <- getCurrentDirectory
       let cabalDevDir = joinPath [dir, "cabal-dev"]
       packages <- getDirectoryContents cabalDevDir
       case filter ("packages-" `isPrefixOf`) packages of
         (packagesFile:_) -> return $ Just (joinPath [cabalDevDir, packagesFile])
         _ -> return Nothing


reloadTestSuite :: MainModuleName
                -> MainModulePath
                -> HsSourcePaths
                -> FS.FilePath
                -> IO ()
reloadTestSuite moduleName modulePath sourcePaths modifiedFile
  | isNotEmacsFile = reloadTestSuite_
  | otherwise = return ()
  where
    isNotEmacsFile = not ('#' `elem` (FS.encodeString $ FS.filename modifiedFile))
    reloadTestSuite_ = do
      -- time <- getZonedTime
      -- putStrLn ""
      -- putStrLn $ replicate 80 '-'
      -- putStrLn $ mconcat [ show time
      --                    , " | "
      --                    , FS.encodeString  modifiedFile]
      -- putStrLn $ replicate 80 '-'
      r <- runInterpreter
      case r of
        Right () -> return ()
        Left e   -> print e

    runInterpreter = do
      mPackageDatabaseFile <- getPackageDatabaseFile
      let args = case mPackageDatabaseFile of
                   Just file -> [ "-i " ++ join ":" sourcePaths
                                , "-package-conf " ++ file]
                   Nothing -> [ "-i " ++ join ":" sourcePaths]
      unsafeRunInterpreterWithArgs args interpreterAction

    interpreterAction = do
      loadModules [modulePath]
      setTopLevelModules [moduleName]
      setImportsQ [("Prelude", Nothing)]
      execution <- interpret "main" (as :: IO ())
      liftIO $ execution
