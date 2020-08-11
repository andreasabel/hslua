{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-|
Module      : Foreign.Lua.Call
Copyright   : © 2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : alpha
Portability : Portable

Marshaling and documenting Haskell functions.
-}
module Foreign.Lua.Call
  ( HaskellFunction (..)
  , toHsFnPrecursor
  , toHsFnPrecursorWithStartIndex
  , applyParameter
  , returnResult
  , Parameter (..)
  , FunctionResult (..)
    -- * Operators
  , (<#>)
  , (=#>)
  , (#?)
    -- * Documentation
  , FunctionDoc (..)
  , ParameterDoc (..)
  , FunctionResultDoc (..)
  , render
    -- * Pushing to Lua
  , pushHaskellFunction
  ) where

import Control.Monad.Except
import Data.Text (Text)
import Foreign.Lua.Core as Lua
import Foreign.Lua.Core.Types (liftLua)
import Foreign.Lua.Peek
import Foreign.Lua.Push
import Foreign.Lua.Raw.Call (hslua_pushhsfunction)
import qualified Data.Text as T

-- | Lua operation with an explicit error type and state (i.e.,
-- without exceptions).
type LuaExcept a = ExceptT PeekError Lua a


--
-- Function components
--

-- | Result of a call to a Haskell function
data FunctionResult a
  = FunctionResult
  { fnResultPusher :: a -> Lua NumResults
  , fnResultDoc :: Maybe FunctionResultDoc
  }

-- | Function parameter.
data Parameter a = Parameter
  { parameterPeeker :: Peeker a
  , parameterDoc    :: ParameterDoc
  }

-- | Haskell equivallent to CFunction, i.e., function callable
-- from Lua.
data HaskellFunction = HaskellFunction
  { callFunction :: Lua NumResults
  , functionDoc :: Maybe FunctionDoc
  }

--
-- Documentation
--

-- | Documentation for a Haskell function
data FunctionDoc = FunctionDoc
  { functionDescription :: Text
  , parameterDocs     :: [ParameterDoc]
  , functionResultDoc :: Maybe FunctionResultDoc
  }
  deriving (Eq, Ord, Show)

-- | Documentation for function parameters.
data ParameterDoc = ParameterDoc
  { parameterName :: Text
  , parameterType :: Text
  , parameterDescription :: Text
  , parameterIsOptional :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Documentation for the result of a function.
data FunctionResultDoc = FunctionResultDoc
  { functionResultType :: Text
  , functionResultDescription :: Text
  }
  deriving (Eq, Ord, Show)


--
-- Haskell function building
--

-- | Helper type used to create 'HaskellFunction's.
data HsFnPrecursor a = HsFnPrecursor
  { hsFnPrecursorAction :: LuaExcept a
  , hsFnMaxParameterIdx :: StackIndex
  , hsFnParameterDocs :: [ParameterDoc]
  }
  deriving (Functor)

-- | Create a HaskellFunction precursor from a pure function.
toHsFnPrecursor :: a -> HsFnPrecursor a
toHsFnPrecursor = toHsFnPrecursorWithStartIndex (StackIndex 0)

toHsFnPrecursorWithStartIndex :: StackIndex -> a -> HsFnPrecursor a
toHsFnPrecursorWithStartIndex idx f = HsFnPrecursor
  { hsFnPrecursorAction = return f
  , hsFnMaxParameterIdx = idx
  , hsFnParameterDocs = mempty
  }

-- | Partially apply a parameter.
applyParameter :: HsFnPrecursor (a -> b)
               -> Parameter a
               -> HsFnPrecursor b
applyParameter bldr param = do
  let action = hsFnPrecursorAction bldr
  let i = hsFnMaxParameterIdx bldr + 1
  let context = "retrieving function argument " <>
        (parameterName . parameterDoc) param
  let nextAction f = withExceptT (pushMsg context) $ do
        x <- ExceptT $ parameterPeeker param i
        return $ f x
  HsFnPrecursor
    { hsFnPrecursorAction = action >>= nextAction
    , hsFnMaxParameterIdx = i
    , hsFnParameterDocs = parameterDoc param : hsFnParameterDocs bldr
    }

-- | Take a 'HaskellFunction' precursor and convert it into a full
-- 'HaskellFunction', using the given 'FunctionResult' to return
-- the result to Lua.
returnResult :: HsFnPrecursor a
             -> FunctionResult a
             -> HaskellFunction
returnResult bldr (FunctionResult push fnResDoc) = HaskellFunction
  { callFunction = do
      result <- runExceptT $ hsFnPrecursorAction bldr
      case result of
        Left err -> do
          pushString $ formatPeekError err
          Lua.error
        Right x -> push x
  , functionDoc = Just $ FunctionDoc
    { functionDescription = ""
    , parameterDocs = reverse $ hsFnParameterDocs bldr
    , functionResultDoc = fnResDoc
    }
  }

-- | Updates the description of a Haskell function. Leaves the function
-- unchanged if it has no documentation.
updateFunctionDescription :: HaskellFunction -> Text -> HaskellFunction
updateFunctionDescription fn desc =
  case functionDoc fn of
    Nothing -> fn
    Just fnDoc ->
      fn { functionDoc = Just $ fnDoc { functionDescription = desc} }

--
-- Operators
--

infixl 8 <#>, =#>, #?

-- | Inline version of @'applyParameter'@.
(<#>) :: HsFnPrecursor (a -> b)
      -> Parameter a
      -> HsFnPrecursor b
(<#>) = applyParameter

-- | Inline version of @'returnResult'@.
(=#>) :: HsFnPrecursor a
      -> FunctionResult a
      -> HaskellFunction
(=#>) = returnResult

-- | Inline version of @'updateFunctionDescription'@.
(#?) :: HaskellFunction -> Text -> HaskellFunction
(#?) = updateFunctionDescription

--
-- Render documentation
--

render :: FunctionDoc -> Text
render (FunctionDoc desc paramDocs resultDoc) =
  (if T.null desc then "" else desc <> "\n\n") <>
  renderParamDocs paramDocs <>
  case resultDoc of
    Nothing -> ""
    Just rd -> mconcat
      [ "\nReturns:\n"
      , renderResultDoc rd
      ]

renderParamDocs :: [ParameterDoc] -> Text
renderParamDocs pds = "Parameters:\n\n" <>
  T.intercalate "\n" (map renderParamDoc pds)

renderParamDoc :: ParameterDoc -> Text
renderParamDoc pd = mconcat
  [ parameterName pd
  ,  "\n:   "
  , parameterDescription pd
  , " (", parameterType pd, ")\n"
  ]

renderResultDoc :: FunctionResultDoc -> Text
renderResultDoc rd = mconcat
  [ functionResultDescription rd
  , " (", functionResultType rd, ")\n"
  ]

--
-- Push to Lua
--

pushHaskellFunction :: HaskellFunction -> Lua ()
pushHaskellFunction fn = do
  errConv <- Lua.errorConversion
  let hsFn = flip (runWithConverter errConv) $ callFunction fn
  liftLua $ \l -> hslua_pushhsfunction l hsFn
