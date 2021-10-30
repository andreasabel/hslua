{-|
Module      : Lua.LPeg
Copyright   : © 2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Haskell bindings to the LPeg Lua package.
-}
module Lua.LPeg
  ( luaopen_lpeg_ptr
  , lpeg_searcher
  ) where

import Foreign.C (peekCStringLen)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import Foreign.Marshal (alloca)
import Lua

-- | Pointer to the function which loads the lpeg library.
foreign import ccall unsafe "lptree.c &luaopen_lpeg"
  luaopen_lpeg_ptr :: CFunction

-- | Name under which the module is loaded.
moduleName :: String
moduleName = "lpeg"

-- | A package searcher to be used with @package.searchers@), just for
-- the "lpeg" module. Returns @nil@ on most inputs, but pushes a
-- function that loads the LPeg module when called with key @"lpeg"@.
lpeg_searcher :: PreCFunction
lpeg_searcher l = 1 <$ do
  alloca $ \lenPtr -> do
    cstr <- lua_tolstring l 1 lenPtr
    if cstr == nullPtr
      then lua_pushnil l
      else do
        cstrLen <- peek lenPtr
        pkg <- peekCStringLen (cstr, fromIntegral cstrLen)
        if pkg /= moduleName
          then lua_pushnil l
          else lua_pushcclosure l luaopen_lpeg_ptr 0
