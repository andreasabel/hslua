{-|
Module      : Main
Copyright   : © 2017-2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

Tests for HsLua.
-}
import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified HsLua.MarshallingTests

main :: IO ()
main = defaultMain tests

-- | HSpec tests
tests :: TestTree
tests = testGroup "HsLua" [HsLua.MarshallingTests.tests]
