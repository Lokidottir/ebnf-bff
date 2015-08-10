module Paths_monadic_ebnf_bff (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/fionanh/.cabal/bin"
libdir     = "/home/fionanh/.cabal/lib/x86_64-linux-ghc-7.8.4/monadic-ebnf-bff-0.1.0.0"
datadir    = "/home/fionanh/.cabal/share/x86_64-linux-ghc-7.8.4/monadic-ebnf-bff-0.1.0.0"
libexecdir = "/home/fionanh/.cabal/libexec"
sysconfdir = "/home/fionanh/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "monadic_ebnf_bff_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "monadic_ebnf_bff_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "monadic_ebnf_bff_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "monadic_ebnf_bff_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "monadic_ebnf_bff_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
