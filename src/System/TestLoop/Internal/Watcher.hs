module System.TestLoop.Internal.Watcher (reloadTestSuite) where

--------------------

import           Control.Monad                       (liftM, sequence)
import           Control.Monad.Trans                 (MonadIO (..))
import           Data.List                           (intercalate, isPrefixOf,
                                                      isSuffixOf, nub)
import           Data.Monoid                         (mconcat, First(..))
import qualified Filesystem.Path                     as FS
import qualified Filesystem.Path.CurrentOS           as FS

--------------------

import           Language.Haskell.Interpreter        (InterpreterError (..), as,
                                                      errMsg, interpret,
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

import           System.TestLoop.Internal.Signal
import           System.TestLoop.Internal.Types
import           System.TestLoop.Util

--------------------------------------------------------------------------------


_getPackageDatabaseFile :: FilePath
                        -> (FilePath -> Bool)
                        -> IO (Maybe FilePath)
_getPackageDatabaseFile folderName isPackageDatabase = do
  cabalDevExists <- doesDirectoryExist folderName
  if (not cabalDevExists)
     then return Nothing
     else do
       dir <- getCurrentDirectory
       let cabalDevDir = joinPath [dir, folderName]
       packages <- getDirectoryContents cabalDevDir
       case filter isPackageDatabase packages of
         (packagesFile:_) -> return $ Just (joinPath [cabalDevDir, packagesFile])

getCabalDevPackageDatabaseFile :: IO (Maybe FilePath)
getCabalDevPackageDatabaseFile =
  _getPackageDatabaseFile "cabal-dev" ("packages-" `isPrefixOf`)

getCabalSandboxPackageDatabaseFile :: IO (Maybe FilePath)
getCabalSandboxPackageDatabaseFile =
  _getPackageDatabaseFile ".cabal-sandbox" ("packages.conf.d" `isSuffixOf`)

getPackageDatabaseFile :: IO (Maybe FilePath)
getPackageDatabaseFile =
  liftM (getFirst . mconcat . map First) $
  sequence [ getCabalSandboxPackageDatabaseFile
           , getCabalDevPackageDatabaseFile ]


reloadTestSuite :: MainModuleName
                -> MainModulePath
                -> HsSourcePaths
                -> FS.FilePath
                -> IO ()
reloadTestSuite moduleName modulePath sourcePaths modifiedFile
  | isNotEmacsFile = do
    reloadTestSuite_
  | otherwise = return ()
  where
    isNotEmacsFile = not ('#' `elem` (FS.encodeString $ FS.filename modifiedFile))
    reloadTestSuite_ = do
      printTimeHeader
      result <- protectHandlers runInterpreter
      case result of
        Left err -> putStrLn "" >> putStrLn (format err)
        Right _ -> return ()

    printTimeHeader = do
      time <- getZonedTime
      putStrLn ""
      putStrLn $ replicate 80 '-'
      putStr "-- "
      putStr (show time)
      putStr " "
      let remaindingWidth = 76 - (length (show time))
      putStrLn $ replicate remaindingWidth '-'

    -- shamelessly stolen from snap-loader-dynamic
    format :: InterpreterError -> String
    format (UnknownError e)   = "Unknown interpreter error:\r\n\r\n" ++ e
    format (NotAllowed e)     = "Interpreter action not allowed:\r\n\r\n" ++ e
    format (GhcException e)   = "GHC error:\r\n\r\n" ++ e
    format (WontCompile errs) = concat ["Compile errors:\r\n\r\n",
                                        intercalate "\r\n" $ nub $ map errMsg errs]

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
