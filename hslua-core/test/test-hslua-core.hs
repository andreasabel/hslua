{-|
Module      : Main
Copyright   : © 2017-2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

Tests for HsLua.Core.
-}
import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified HsLua.CoreTests

-- | Runs tests.
main :: IO ()
main = defaultMain tests

-- | HsLua core tests.
tests :: TestTree
tests = testGroup "hslua-core" [HsLua.CoreTests.tests]
