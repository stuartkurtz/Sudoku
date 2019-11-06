{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_nstate (
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
version = Version [0,2,0,5] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Volumes/Home/stuart/.cabal/bin"
libdir     = "/Volumes/Home/stuart/.cabal/lib/x86_64-osx-ghc-8.6.5/nstate-0.2.0.5-inplace"
dynlibdir  = "/Volumes/Home/stuart/.cabal/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Volumes/Home/stuart/.cabal/share/x86_64-osx-ghc-8.6.5/nstate-0.2.0.5"
libexecdir = "/Volumes/Home/stuart/.cabal/libexec/x86_64-osx-ghc-8.6.5/nstate-0.2.0.5"
sysconfdir = "/Volumes/Home/stuart/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "nstate_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "nstate_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "nstate_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "nstate_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "nstate_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "nstate_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
