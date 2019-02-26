{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_SpelPrototyp8 (
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

bindir     = "/home/fryn4538/.cabal/bin"
libdir     = "/home/fryn4538/.cabal/lib/x86_64-linux-ghc-8.0.2/SpelPrototyp8-0.1.0.0"
dynlibdir  = "/home/fryn4538/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/fryn4538/.cabal/share/x86_64-linux-ghc-8.0.2/SpelPrototyp8-0.1.0.0"
libexecdir = "/home/fryn4538/.cabal/libexec"
sysconfdir = "/home/fryn4538/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "SpelPrototyp8_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "SpelPrototyp8_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "SpelPrototyp8_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "SpelPrototyp8_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SpelPrototyp8_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "SpelPrototyp8_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
