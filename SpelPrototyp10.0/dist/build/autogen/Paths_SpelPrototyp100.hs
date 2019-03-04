{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_SpelPrototyp100 (
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

bindir     = "/home/august/.cabal/bin"
libdir     = "/home/august/.cabal/lib/x86_64-linux-ghc-8.0.2/SpelPrototyp100-0.1.0.0"
dynlibdir  = "/home/august/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/august/.cabal/share/x86_64-linux-ghc-8.0.2/SpelPrototyp100-0.1.0.0"
libexecdir = "/home/august/.cabal/libexec"
sysconfdir = "/home/august/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "SpelPrototyp100_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "SpelPrototyp100_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "SpelPrototyp100_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "SpelPrototyp100_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SpelPrototyp100_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "SpelPrototyp100_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
