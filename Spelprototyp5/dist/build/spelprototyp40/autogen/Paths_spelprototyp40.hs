{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_spelprototyp40 (
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
libdir     = "/Users/augustbredberg/.cabal/lib/x86_64-osx-ghc-8.6.3/spelprototyp40-0.1.0.0-ExkC35fJmCq6uWV9y1iC3O-spelprototyp40"
dynlibdir  = "/Users/augustbredberg/.cabal/lib/x86_64-osx-ghc-8.6.3"
datadir    = "/Users/augustbredberg/.cabal/share/x86_64-osx-ghc-8.6.3/spelprototyp40-0.1.0.0"
libexecdir = "/Users/augustbredberg/.cabal/libexec/x86_64-osx-ghc-8.6.3/spelprototyp40-0.1.0.0"
sysconfdir = "/Users/augustbredberg/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "spelprototyp40_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "spelprototyp40_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "spelprototyp40_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "spelprototyp40_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "spelprototyp40_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "spelprototyp40_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
