{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeApplications  #-}
{-| Lua interpreter with many preloaded libraries.
-}
module Main where

import Data.Maybe (listToMaybe)
import HsLua.Core
import HsLua.Packaging (preloadModule)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import qualified HsLua.Module.DocLayout as DocLayout
import qualified HsLua.Module.Path as Path
import qualified HsLua.Module.System as System

main :: IO ()
main = do
  script <- listToMaybe <$> getArgs
  luaStatus <- run @HsLua.Core.Exception $ do
    openlibs             -- load the default Lua packages
    preloadModule DocLayout.documentedModule
    preloadModule Path.documentedModule
    preloadModule System.documentedModule
    dofileTrace script >>= \case
      OK -> pure OK
      s  -> do
        throwErrorAsException
        pure s
  if luaStatus == OK
    then exitSuccess
    else exitFailure
