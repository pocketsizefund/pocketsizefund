{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_shared (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/chrisaddy/projects/pocketsizefund/pocketsizefund/platform/datamanager/.stack-work/install/aarch64-osx/5c55c94ea8915a5a37c37223a829a45fd667b1807b21af212260b874c21a5ebf/9.8.4/bin"
libdir     = "/Users/chrisaddy/projects/pocketsizefund/pocketsizefund/platform/datamanager/.stack-work/install/aarch64-osx/5c55c94ea8915a5a37c37223a829a45fd667b1807b21af212260b874c21a5ebf/9.8.4/lib/aarch64-osx-ghc-9.8.4/shared-0.1.0.0-4wJ0Pn5z0CSDKWJWjX39m1"
dynlibdir  = "/Users/chrisaddy/projects/pocketsizefund/pocketsizefund/platform/datamanager/.stack-work/install/aarch64-osx/5c55c94ea8915a5a37c37223a829a45fd667b1807b21af212260b874c21a5ebf/9.8.4/lib/aarch64-osx-ghc-9.8.4"
datadir    = "/Users/chrisaddy/projects/pocketsizefund/pocketsizefund/platform/datamanager/.stack-work/install/aarch64-osx/5c55c94ea8915a5a37c37223a829a45fd667b1807b21af212260b874c21a5ebf/9.8.4/share/aarch64-osx-ghc-9.8.4/shared-0.1.0.0"
libexecdir = "/Users/chrisaddy/projects/pocketsizefund/pocketsizefund/platform/datamanager/.stack-work/install/aarch64-osx/5c55c94ea8915a5a37c37223a829a45fd667b1807b21af212260b874c21a5ebf/9.8.4/libexec/aarch64-osx-ghc-9.8.4/shared-0.1.0.0"
sysconfdir = "/Users/chrisaddy/projects/pocketsizefund/pocketsizefund/platform/datamanager/.stack-work/install/aarch64-osx/5c55c94ea8915a5a37c37223a829a45fd667b1807b21af212260b874c21a5ebf/9.8.4/etc"

getBinDir     = catchIO (getEnv "shared_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "shared_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "shared_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "shared_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "shared_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "shared_sysconfdir") (\_ -> return sysconfdir)



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
