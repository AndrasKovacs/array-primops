
{-# LANGUAGE
    MagicHash, ForeignFunctionInterface,
    GHCForeignImportPrim, UnliftedFFITypes #-}

module GHC.Prim.Array where

import GHC.Prim (Int#, Array#, Any)

-- | Prepend an element to an array.
foreign import prim "consArray" consArray#
  :: Any -> Array# a -> Array# a

-- | Append an element to the end of an array.
foreign import prim "snocArray" snocArray#
  :: Array# a -> Any -> Array# a

-- | Insert new element at position.
foreign import prim "insertArray" insertArray#
  :: Int# -> Any -> Array# a -> Array# a

-- | Delete element at position. 
foreign import prim "deleteArray" deleteArray#
  :: Int# -> Array# a -> Array# a
