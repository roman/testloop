module System.TestLoop.Internal.Watcher (reloadTestSuite) where

--------------------

import Control.Monad.Trans (MonadIO (..))
import Data.List (intercalate, nub)
import qualified Filesystem.Path           as FS
import qualified Filesystem.Path.CurrentOS as FS

--------------------

import Language.Haskell.Interpreter (InterpreterError (..), as, errMsg,
                                     interpret, loadModules, setImportsQ,
                                     setTopLevelModules)
import Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)

--------------------

import Data.Time.LocalTime (getZonedTime)

--------------------

import System.TestLoop.Internal.Signal
import System.TestLoop.Internal.Types
import System.TestLoop.Util

--------------------------------------------------------------------------------

reloadTestSuite :: Maybe PackageDbFile
                -> MainModuleName
                -> MainModulePath
                -> HsSourcePaths
                -> FS.FilePath
                -> IO ()
reloadTestSuite pkgDb moduleName modulePath sourcePaths modifiedFile
  | isNotEmacsFile = reloadTestSuite_
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

    runInterpreter =
      let args = case pkgDb of
                   Just file -> [ "-i " ++ join ":" sourcePaths
                                , "-package-conf " ++ file]
                   Nothing -> [ "-i " ++ join ":" sourcePaths]
      in unsafeRunInterpreterWithArgs args interpreterAction

    interpreterAction = do
      loadModules [modulePath]
      setTopLevelModules [moduleName]
      setImportsQ [("Prelude", Nothing)]
      execution <- interpret "main" (as :: IO ())
      liftIO $ execution
