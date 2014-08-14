module Paths_opentypes (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "C:\\Users\\MJP\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\MJP\\AppData\\Roaming\\cabal\\opentypes-0.0.1\\ghc-7.6.3"
datadir    = "C:\\Users\\MJP\\AppData\\Roaming\\cabal\\opentypes-0.0.1"
libexecdir = "C:\\Users\\MJP\\AppData\\Roaming\\cabal\\opentypes-0.0.1"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "opentypes_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "opentypes_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "opentypes_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "opentypes_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
