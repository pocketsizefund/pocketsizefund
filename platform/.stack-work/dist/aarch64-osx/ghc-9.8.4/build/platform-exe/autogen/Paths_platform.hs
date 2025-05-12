{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_platform (
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
version = Version [2025,4,14,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/chrisaddy/projects/pocketsizefund/pocketsizefund/platform/.stack-work/install/aarch64-osx/718f116867b7da94dd13ac6bc02a2257dfa967104d5e6ef51a70c8ef769abd24/9.8.4/bin"
libdir     = "/Users/chrisaddy/projects/pocketsizefund/pocketsizefund/platform/.stack-work/install/aarch64-osx/718f116867b7da94dd13ac6bc02a2257dfa967104d5e6ef51a70c8ef769abd24/9.8.4/lib/aarch64-osx-ghc-9.8.4/platform-2025.4.14.1-8UQN6k924zS1nT9H7Ekno2-platform-exe"
dynlibdir  = "/Users/chrisaddy/projects/pocketsizefund/pocketsizefund/platform/.stack-work/install/aarch64-osx/718f116867b7da94dd13ac6bc02a2257dfa967104d5e6ef51a70c8ef769abd24/9.8.4/lib/aarch64-osx-ghc-9.8.4"
datadir    = "/Users/chrisaddy/projects/pocketsizefund/pocketsizefund/platform/.stack-work/install/aarch64-osx/718f116867b7da94dd13ac6bc02a2257dfa967104d5e6ef51a70c8ef769abd24/9.8.4/share/aarch64-osx-ghc-9.8.4/platform-2025.4.14.1"
libexecdir = "/Users/chrisaddy/projects/pocketsizefund/pocketsizefund/platform/.stack-work/install/aarch64-osx/718f116867b7da94dd13ac6bc02a2257dfa967104d5e6ef51a70c8ef769abd24/9.8.4/libexec/aarch64-osx-ghc-9.8.4/platform-2025.4.14.1"
sysconfdir = "/Users/chrisaddy/projects/pocketsizefund/pocketsizefund/platform/.stack-work/install/aarch64-osx/718f116867b7da94dd13ac6bc02a2257dfa967104d5e6ef51a70c8ef769abd24/9.8.4/etc"

getBinDir     = catchIO (getEnv "platform_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "platform_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "platform_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "platform_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "platform_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "platform_sysconfdir") (\_ -> return sysconfdir)



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
