module TestLoop.Internal.Cabal (parseCabalFile) where

--------------------

import           Control.Applicative                   ((<$>), (<*>))
import           Data.List                             (isInfixOf)
import           Data.Monoid                           (First (..), mconcat)
import           System.Directory                      (getCurrentDirectory,
                                                        getDirectoryContents,
                                                        getHomeDirectory)
import           System.Environment                    (getArgs)
import           System.FilePath                       (joinPath, takeDirectory)

--------------------

import           Distribution.PackageDescription       (CondTree (..), GenericPackageDescription (..),
                                                        PackageDescription (..),
                                                        TestSuite (..),
                                                        TestSuiteInterface (..),
                                                        condTreeData,
                                                        hsSourceDirs,
                                                        testBuildInfo)
import           Distribution.PackageDescription.Parse (readPackageDescription)
import           Distribution.Verbosity                (normal)

--------------------

import           TestLoop.Internal.Types

--------------------------------------------------------------------------------

getTestSuiteToRun :: IO (Maybe String)
getTestSuiteToRun = do
    args <- getArgs
    case args of
      (x:_) -> return (Just x)
      _ -> return Nothing

parseTestSuiteInfo :: Maybe String
                   -> (String, CondTree a b TestSuite)
                   -> Maybe (String, String, [String])
parseTestSuiteInfo (Just inputName) (name, CondNode { condTreeData=testSuite })
    | inputName == name =
      case testInterface testSuite of
        TestSuiteExeV10 _ file -> Just (name, file, hsSourceDirs $ testBuildInfo testSuite)
        _ -> Nothing
    | otherwise = Nothing
parseTestSuiteInfo Nothing input@(name, _) = parseTestSuiteInfo (Just name) input

getCabalFilePathFrom :: FilePath -> IO (Maybe FilePath)
getCabalFilePathFrom originalPath =
    getHomeDirectory >>= loop originalPath
  where
    loop currentPath finalPath = do
      if currentPath == finalPath
        then return Nothing
        else do
          contents <- getDirectoryContents currentPath
          case dropWhile (not . (".cabal" `isInfixOf`)) contents of
            [] -> loop (takeDirectory currentPath) finalPath
            (result:_) -> return $ Just (joinPath [currentPath, result])


getCabalFilePath :: IO (Maybe FilePath)
getCabalFilePath = do
    getCurrentDirectory >>= getCabalFilePathFrom

parseCabalFile_ :: Maybe String
                -> GenericPackageDescription
                -> Maybe (String, String, [String])
parseCabalFile_ testSuiteName genericPackDesc =
    getFirst . mconcat $ map (First . parseTestSuiteInfo testSuiteName)
                             (condTestSuites genericPackDesc)

parseCabalFile :: IO (TestSuiteName, MainModuleName, HsSourcePaths)
parseCabalFile = do
    mcabalFilePath <- getCabalFilePath
    case mcabalFilePath of
      Just cabalFilePath -> do
        result <- parseCabalFile_ <$> getTestSuiteToRun
                                  <*> readPackageDescription normal cabalFilePath
        maybe (error $ msg ++ cabalFilePath)
              return
              result
      Nothing ->
          error "Couldn't find a cabal file in this directory"
  where
    msg = mconcat [ "You need to have at least one test-suite "
                  , "with type == exitcode-stdio-1.0 on "]
