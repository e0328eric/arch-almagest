{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_battery_low (
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
version = Version [1,0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/almagest/GitHub/backup/xmonad/battery-low/.stack-work/install/x86_64-linux-tinfo6/0cceb95eab3f3b9d8a80f6ca33f0caffc736c0731ff2b773dfe62b90a798635e/8.8.3/bin"
libdir     = "/home/almagest/GitHub/backup/xmonad/battery-low/.stack-work/install/x86_64-linux-tinfo6/0cceb95eab3f3b9d8a80f6ca33f0caffc736c0731ff2b773dfe62b90a798635e/8.8.3/lib/x86_64-linux-ghc-8.8.3/battery-low-1.0.0.0-4I8FXZi5YnRGT14UXkKiDU-battery-low"
dynlibdir  = "/home/almagest/GitHub/backup/xmonad/battery-low/.stack-work/install/x86_64-linux-tinfo6/0cceb95eab3f3b9d8a80f6ca33f0caffc736c0731ff2b773dfe62b90a798635e/8.8.3/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/home/almagest/GitHub/backup/xmonad/battery-low/.stack-work/install/x86_64-linux-tinfo6/0cceb95eab3f3b9d8a80f6ca33f0caffc736c0731ff2b773dfe62b90a798635e/8.8.3/share/x86_64-linux-ghc-8.8.3/battery-low-1.0.0.0"
libexecdir = "/home/almagest/GitHub/backup/xmonad/battery-low/.stack-work/install/x86_64-linux-tinfo6/0cceb95eab3f3b9d8a80f6ca33f0caffc736c0731ff2b773dfe62b90a798635e/8.8.3/libexec/x86_64-linux-ghc-8.8.3/battery-low-1.0.0.0"
sysconfdir = "/home/almagest/GitHub/backup/xmonad/battery-low/.stack-work/install/x86_64-linux-tinfo6/0cceb95eab3f3b9d8a80f6ca33f0caffc736c0731ff2b773dfe62b90a798635e/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "battery_low_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "battery_low_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "battery_low_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "battery_low_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "battery_low_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "battery_low_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
