{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_xmonad_config (
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
version = Version [0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/shadow/.cabal/bin"
libdir     = "/home/shadow/.cabal/lib/x86_64-linux-ghc-8.8.4/xmonad-config-0.0-inplace-xmonad-config"
dynlibdir  = "/home/shadow/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/shadow/.cabal/share/x86_64-linux-ghc-8.8.4/xmonad-config-0.0"
libexecdir = "/home/shadow/.cabal/libexec/x86_64-linux-ghc-8.8.4/xmonad-config-0.0"
sysconfdir = "/home/shadow/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "xmonad_config_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "xmonad_config_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "xmonad_config_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "xmonad_config_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "xmonad_config_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "xmonad_config_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
