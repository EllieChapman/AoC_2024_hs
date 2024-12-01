{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Aoc2024 (
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
bindir     = "/home/echapman/AoC_2024_hs/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/232862e91b933fa725d6224f4a49addabece274c16011b0fb982e3c551dc0442/9.2.7/bin"
libdir     = "/home/echapman/AoC_2024_hs/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/232862e91b933fa725d6224f4a49addabece274c16011b0fb982e3c551dc0442/9.2.7/lib/x86_64-linux-ghc-9.2.7/Aoc2024-0.1.0.0-1vMSGYRIuPr6HgwO3TMsna"
dynlibdir  = "/home/echapman/AoC_2024_hs/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/232862e91b933fa725d6224f4a49addabece274c16011b0fb982e3c551dc0442/9.2.7/lib/x86_64-linux-ghc-9.2.7"
datadir    = "/home/echapman/AoC_2024_hs/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/232862e91b933fa725d6224f4a49addabece274c16011b0fb982e3c551dc0442/9.2.7/share/x86_64-linux-ghc-9.2.7/Aoc2024-0.1.0.0"
libexecdir = "/home/echapman/AoC_2024_hs/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/232862e91b933fa725d6224f4a49addabece274c16011b0fb982e3c551dc0442/9.2.7/libexec/x86_64-linux-ghc-9.2.7/Aoc2024-0.1.0.0"
sysconfdir = "/home/echapman/AoC_2024_hs/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/232862e91b933fa725d6224f4a49addabece274c16011b0fb982e3c551dc0442/9.2.7/etc"

getBinDir     = catchIO (getEnv "Aoc2024_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Aoc2024_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Aoc2024_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Aoc2024_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Aoc2024_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Aoc2024_sysconfdir") (\_ -> return sysconfdir)




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
