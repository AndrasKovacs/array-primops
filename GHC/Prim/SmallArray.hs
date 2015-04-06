
{-# LANGUAGE
    MagicHash, ForeignFunctionInterface,
    GHCForeignImportPrim, UnliftedFFITypes #-}

module GHC.Prim.SmallArray where

import GHC.Prim (Int#, SmallArray#, Any)

-- | Prepend an element to an array.
foreign import prim "consSmallArray" consSmallArray#
  :: Any -> SmallArray# a -> SmallArray# a

-- | Append an element to the end of an array.
foreign import prim "snocSmallArray" snocSmallArray#
  :: SmallArray# a -> Any -> SmallArray# a

-- | Insert new element at position.
foreign import prim "insertSmallArray" insertSmallArray#
  :: Int# -> Any -> SmallArray# a -> SmallArray# a

-- | Delete element at position. 
foreign import prim "deleteSmallArray" deleteSmallArray#
  :: Int# -> SmallArray# a -> SmallArray# a
