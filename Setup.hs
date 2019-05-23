{-# LANGUAGE CPP #-}

import           Data.Char (isDigit)
import           Data.List (intercalate)
import           Data.Monoid ((<>))

import           Distribution.PackageDescription
import           Distribution.Verbosity
import           Distribution.Simple
import           Distribution.Simple.Setup (BuildFlags(..), ReplFlags(..), TestFlags(..), fromFlag)
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.BuildPaths (autogenModulesDir)
import           Distribution.Simple.Utils (createDirectoryIfMissingVerbose, rewriteFile, rawSystemStdout)

#ifndef MIN_VERSION_Cabal
#if __GLASGOW_HASKELL__ <= 710
-- GHC 7.10 and earlier do not support the MIN_VERSION_Cabal macro.
#define MIN_VERSION_Cabal(a,b,c) 0
#endif
#endif

#if !MIN_VERSION_Cabal(2,0,0)
import           Data.Version (showVersion)
#endif


--
--                                                   /===-_---~~~~~~~~~------____
--                                                  |===-~___                _,-'
--                   -==\\                         `//~\\   ~~~~`---.___.-~~
--               ______-==|                         | |  \\           _-~`
--         __--~~~  ,-/-==\\                        | |   `\        ,'
--      _-~       /'    |  \\                      / /      \      /
--    .'        /       |   \\                   /' /        \   /'
--   /  ____  /         |    \`\.__/-~~ ~ \ _ _/'  /          \/'
--  /-'~    ~~~~~---__  |     ~-/~         ( )   /'        _--~`
--                    \_|      /        _)   ;  ),   __--~~
--                      '~~--_/      _-~/-  / \   '-~ \
--                     {\__--_/}    / \\_>- )<__\      \
--                     /'   (_/  _-~  | |__>--<__|      |
--                    |0  0 _/) )-~     | |__>--<__|      |
--                    / /~ ,_/       / /__>---<__/      |
--                   o o _//        /-~_>---<__-~      /
--                   (^(~          /~_>---<__-      _-~
--                  ,/|           /__>--<__/     _-~
--               ,//('(          |__>--<__|     /                  .----_
--              ( ( '))          |__>--<__|    |                 /' _---_~\
--           `-)) )) (           |__>--<__|    |               /'  /     ~\`\
--          ,/,'//( (             \__>--<__\    \            /'  //        ||
--        ,( ( ((, ))              ~-__>--<_~-_  ~--____---~' _/'/        /'
--      `~/  )` ) ,/|                 ~-_~>--<_/-__       __-~ _/
--    ._-~//( )/ )) `                    ~~-'_/_/ /~~~~~~~__--~
--     ;'( ')/ ,)(                              ~~~~~~~~~~
--    ' ') '( (/
--      '   '  `
--
--  NOTE  This file differs from the standard Ambiata Setup.hs in that we use
--  NOTE  'autoconfUserHooks' below instead of 'simpleUserHooks'. Be sure to
--  NOTE  take this in to account when upgrading.
--
main :: IO ()
main =
  let hooks = autoconfUserHooks
   in defaultMainWithHooks hooks {
     preConf = \args flags -> do
       createDirectoryIfMissingVerbose silent True "gen"
       (preConf hooks) args flags
   , sDistHook = \pd mlbi uh flags -> do
       genBuildInfo silent pd
       (sDistHook hooks) pd mlbi uh flags
   , buildHook = \pd lbi uh flags -> do
       genBuildInfo (fromFlag $ buildVerbosity flags) pd
       (buildHook hooks) pd lbi uh flags
   , replHook = \pd lbi uh flags args -> do
       genBuildInfo (fromFlag $ replVerbosity flags) pd
       (replHook hooks) pd lbi uh flags args
   , testHook = \args pd lbi uh flags -> do
       genBuildInfo (fromFlag $ testVerbosity flags) pd
       (testHook hooks) args pd lbi uh flags
   }

genBuildInfo :: Verbosity -> PackageDescription -> IO ()
genBuildInfo verbosity pkg = do
  createDirectoryIfMissingVerbose verbosity True "gen"
  let pname = unPackageName . pkgName . package $ pkg
      version = pkgVersion . package $ pkg
      name = "BuildInfo_" ++ (map (\c -> if c == '-' then '_' else c) pname)
      targetHs = "gen/" ++ name ++ ".hs"
      targetText = "gen/version.txt"
  t <- timestamp verbosity
  gv <- gitVersion verbosity
  let v = showVersion version
  let buildVersion = intercalate "-" [v, t, gv]
  rewriteFile targetHs $ unlines [
      "module " ++ name ++ " where"
    , "import Prelude"
    , "data RuntimeBuildInfo = RuntimeBuildInfo { buildVersion :: String, timestamp :: String, gitVersion :: String }"
    , "buildInfo :: RuntimeBuildInfo"
    , "buildInfo = RuntimeBuildInfo \"" ++ v ++ "\" \"" ++ t ++ "\" \"" ++ gv ++ "\""
    , "buildInfoVersion :: String"
    , "buildInfoVersion = \"" ++ buildVersion ++ "\""
    ]
  rewriteFile targetText buildVersion

gitVersion :: Verbosity -> IO String
gitVersion verbosity = do
  ver <- rawSystemStdout verbosity "git" ["log", "--pretty=format:%h", "-n", "1"]
  notModified <- ((>) 1 . length) `fmap` rawSystemStdout verbosity "git" ["status", "--porcelain"]
  return $ ver ++ if notModified then "" else "-M"

timestamp :: Verbosity -> IO String
timestamp verbosity =
  rawSystemStdout verbosity "date" ["+%Y%m%d%H%M%S"] >>= \s ->
    case splitAt 14 s of
      (d, n : []) ->
        if (length d == 14 && filter isDigit d == d)
          then return d
          else fail $ "date has failed to produce the correct format [" <> s <> "]."
      _ ->
        fail $ "date has failed to produce a date long enough [" <> s <> "]."
