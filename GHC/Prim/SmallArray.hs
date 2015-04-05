
{-# LANGUAGE
    MagicHash, ForeignFunctionInterface,
    GHCForeignImportPrim, UnliftedFFITypes #-}

module GHC.Prim.SmallArray where

import GHC.Prim (Int#, SmallArray#, Any)

foreign import prim "consSmallArray" consSmallArray#
  :: Any -> SmallArray# a -> SmallArray# a

foreign import prim "snocSmallArray" snocSmallArray#
  :: SmallArray# a -> Any -> SmallArray# a

foreign import prim "insertSmallArray" insertSmallArray#
  :: Int# -> Any -> SmallArray# a -> SmallArray# a

foreign import prim "deleteSmallArray" deleteSmallArray#
  :: Int# -> SmallArray# a -> SmallArray# a








