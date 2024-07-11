{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_luna (
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
bindir     = "/Users/gemlorg/studies/y3/jpp/inter/heorhii_lopatin/.stack-work/install/aarch64-osx/0401c71734476ffd39cb1d9cebf1331430e253d5fa507f48f828623f64140280/9.6.4/bin"
libdir     = "/Users/gemlorg/studies/y3/jpp/inter/heorhii_lopatin/.stack-work/install/aarch64-osx/0401c71734476ffd39cb1d9cebf1331430e253d5fa507f48f828623f64140280/9.6.4/lib/aarch64-osx-ghc-9.6.4/luna-0.1.0.0-GiUV18mm9ApK6W3lR3AfH8-luna-exe"
dynlibdir  = "/Users/gemlorg/studies/y3/jpp/inter/heorhii_lopatin/.stack-work/install/aarch64-osx/0401c71734476ffd39cb1d9cebf1331430e253d5fa507f48f828623f64140280/9.6.4/lib/aarch64-osx-ghc-9.6.4"
datadir    = "/Users/gemlorg/studies/y3/jpp/inter/heorhii_lopatin/.stack-work/install/aarch64-osx/0401c71734476ffd39cb1d9cebf1331430e253d5fa507f48f828623f64140280/9.6.4/share/aarch64-osx-ghc-9.6.4/luna-0.1.0.0"
libexecdir = "/Users/gemlorg/studies/y3/jpp/inter/heorhii_lopatin/.stack-work/install/aarch64-osx/0401c71734476ffd39cb1d9cebf1331430e253d5fa507f48f828623f64140280/9.6.4/libexec/aarch64-osx-ghc-9.6.4/luna-0.1.0.0"
sysconfdir = "/Users/gemlorg/studies/y3/jpp/inter/heorhii_lopatin/.stack-work/install/aarch64-osx/0401c71734476ffd39cb1d9cebf1331430e253d5fa507f48f828623f64140280/9.6.4/etc"

getBinDir     = catchIO (getEnv "luna_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "luna_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "luna_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "luna_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "luna_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "luna_sysconfdir") (\_ -> return sysconfdir)



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
