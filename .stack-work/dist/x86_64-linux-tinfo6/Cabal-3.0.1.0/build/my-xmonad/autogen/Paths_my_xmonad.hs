{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_my_xmonad (
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

bindir     = "/home/shadow/.xmonad/.stack-work/install/x86_64-linux-tinfo6/dee1175974aafeb681972f2f531f78b45bdeb5db304cd93fd21a6a1d7675bd4f/8.8.4/bin"
libdir     = "/home/shadow/.xmonad/.stack-work/install/x86_64-linux-tinfo6/dee1175974aafeb681972f2f531f78b45bdeb5db304cd93fd21a6a1d7675bd4f/8.8.4/lib/x86_64-linux-ghc-8.8.4/my-xmonad-0.1.0.0-ATBfkpLaQLJDtfMXLJWN1U-my-xmonad"
dynlibdir  = "/home/shadow/.xmonad/.stack-work/install/x86_64-linux-tinfo6/dee1175974aafeb681972f2f531f78b45bdeb5db304cd93fd21a6a1d7675bd4f/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/shadow/.xmonad/.stack-work/install/x86_64-linux-tinfo6/dee1175974aafeb681972f2f531f78b45bdeb5db304cd93fd21a6a1d7675bd4f/8.8.4/share/x86_64-linux-ghc-8.8.4/my-xmonad-0.1.0.0"
libexecdir = "/home/shadow/.xmonad/.stack-work/install/x86_64-linux-tinfo6/dee1175974aafeb681972f2f531f78b45bdeb5db304cd93fd21a6a1d7675bd4f/8.8.4/libexec/x86_64-linux-ghc-8.8.4/my-xmonad-0.1.0.0"
sysconfdir = "/home/shadow/.xmonad/.stack-work/install/x86_64-linux-tinfo6/dee1175974aafeb681972f2f531f78b45bdeb5db304cd93fd21a6a1d7675bd4f/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "my_xmonad_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "my_xmonad_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "my_xmonad_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "my_xmonad_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "my_xmonad_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "my_xmonad_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
