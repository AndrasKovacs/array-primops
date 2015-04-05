
{-# LANGUAGE
    MagicHash, ForeignFunctionInterface,
    GHCForeignImportPrim, UnliftedFFITypes #-}

module GHC.Prim.Array where

import GHC.Prim (Int#, Array#, Any)

foreign import prim "consArray" consArray#
  :: Any -> Array# a -> Array# a

foreign import prim "snocArray" snocArray#
  :: Array# a -> Any -> Array# a

foreign import prim "insertArray" insertArray#
  :: Int# -> Any -> Array# a -> Array# a

foreign import prim "deleteArray" deleteArray#
  :: Int# -> Array# a -> Array# a








