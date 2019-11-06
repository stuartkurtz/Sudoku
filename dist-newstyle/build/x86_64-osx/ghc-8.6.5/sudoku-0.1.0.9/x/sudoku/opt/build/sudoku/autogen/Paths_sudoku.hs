{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_sudoku (
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
version = Version [0,1,0,9] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Volumes/Home/stuart/.cabal/bin"
libdir     = "/Volumes/Home/stuart/.cabal/lib/x86_64-osx-ghc-8.6.5/sudoku-0.1.0.9-inplace-sudoku"
dynlibdir  = "/Volumes/Home/stuart/.cabal/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Volumes/Home/stuart/.cabal/share/x86_64-osx-ghc-8.6.5/sudoku-0.1.0.9"
libexecdir = "/Volumes/Home/stuart/.cabal/libexec/x86_64-osx-ghc-8.6.5/sudoku-0.1.0.9"
sysconfdir = "/Volumes/Home/stuart/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sudoku_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sudoku_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "sudoku_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "sudoku_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sudoku_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sudoku_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
