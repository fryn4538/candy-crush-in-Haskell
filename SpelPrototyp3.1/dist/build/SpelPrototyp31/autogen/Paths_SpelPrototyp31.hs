{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_SpelPrototyp31 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/augustbredberg/.cabal/bin"
libdir     = "/Users/augustbredberg/.cabal/lib/x86_64-osx-ghc-8.6.3/SpelPrototyp31-0.1.0.0-CUC5PLsBz7KjH8E56Yu7-SpelPrototyp31"
dynlibdir  = "/Users/augustbredberg/.cabal/lib/x86_64-osx-ghc-8.6.3"
datadir    = "/Users/augustbredberg/.cabal/share/x86_64-osx-ghc-8.6.3/SpelPrototyp31-0.1.0.0"
libexecdir = "/Users/augustbredberg/.cabal/libexec/x86_64-osx-ghc-8.6.3/SpelPrototyp31-0.1.0.0"
sysconfdir = "/Users/augustbredberg/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "SpelPrototyp31_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "SpelPrototyp31_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "SpelPrototyp31_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "SpelPrototyp31_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SpelPrototyp31_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "SpelPrototyp31_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
