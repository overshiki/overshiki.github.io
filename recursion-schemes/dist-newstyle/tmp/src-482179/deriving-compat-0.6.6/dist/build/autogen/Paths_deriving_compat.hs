{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_deriving_compat (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [0,6,6] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/le/.cabal/store/ghc-9.2.8/deriving-compat-0.6.6-7bf52ad3d1cee047d3140361dfa26e7cfc3a4f11a4d12e33de7bdc5b3d3d4e78/bin"
libdir     = "/home/le/.cabal/store/ghc-9.2.8/deriving-compat-0.6.6-7bf52ad3d1cee047d3140361dfa26e7cfc3a4f11a4d12e33de7bdc5b3d3d4e78/lib"
dynlibdir  = "/home/le/.cabal/store/ghc-9.2.8/deriving-compat-0.6.6-7bf52ad3d1cee047d3140361dfa26e7cfc3a4f11a4d12e33de7bdc5b3d3d4e78/lib"
datadir    = "/home/le/.cabal/store/ghc-9.2.8/deriving-compat-0.6.6-7bf52ad3d1cee047d3140361dfa26e7cfc3a4f11a4d12e33de7bdc5b3d3d4e78/share"
libexecdir = "/home/le/.cabal/store/ghc-9.2.8/deriving-compat-0.6.6-7bf52ad3d1cee047d3140361dfa26e7cfc3a4f11a4d12e33de7bdc5b3d3d4e78/libexec"
sysconfdir = "/home/le/.cabal/store/ghc-9.2.8/deriving-compat-0.6.6-7bf52ad3d1cee047d3140361dfa26e7cfc3a4f11a4d12e33de7bdc5b3d3d4e78/etc"

getBinDir     = catchIO (getEnv "deriving_compat_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "deriving_compat_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "deriving_compat_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "deriving_compat_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "deriving_compat_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "deriving_compat_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
