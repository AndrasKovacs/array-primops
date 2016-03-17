
{-# LANGUAGE
    MagicHash, ForeignFunctionInterface,
    GHCForeignImportPrim, UnliftedFFITypes #-}

module GHC.Prim.SmallArray (
    consSmallArray#
  , snocSmallArray#
  , snocSmallArrayWithPadding#
  , insertSmallArray#
  , deleteSmallArray#
  ) where

import GHC.Prim

-- | Prepend an element to an array.
foreign import prim "consSmallArray" consSmallArray_
  :: Any -> SmallArray# a -> SmallArray# a

-- | Append an element to the end of an array.
foreign import prim "snocSmallArray" snocSmallArray_
  :: SmallArray# a -> Any -> SmallArray# a

foreign import prim "snocSmallArrayWithPadding" snocSmallArrayWithPadding_
  :: Int# -> Any -> SmallArray# a -> Any -> SmallArray# a

-- | Insert new element at position.
foreign import prim "insertSmallArray" insertSmallArray_
  :: Int# -> Any -> SmallArray# a -> SmallArray# a

-- | Delete element at position.
foreign import prim "deleteSmallArray" deleteSmallArray#
  :: Int# -> SmallArray# a -> SmallArray# a

consSmallArray# :: a -> SmallArray# a -> SmallArray# a
consSmallArray# a arr = consSmallArray_ (unsafeCoerce# a) arr
{-# inline consSmallArray# #-}

snocSmallArray# :: SmallArray# a -> a -> SmallArray# a
snocSmallArray# arr a = snocSmallArray_ arr (unsafeCoerce# a)
{-# inline snocSmallArray# #-}

snocSmallArrayWithPadding# :: Int# -> a -> SmallArray# a -> a -> SmallArray# a
snocSmallArrayWithPadding# pad padElem arr a =
  snocSmallArrayWithPadding_ pad (unsafeCoerce# padElem) arr (unsafeCoerce# a)
{-# inline snocSmallArrayWithPadding# #-}

insertSmallArray# :: Int# -> a -> SmallArray# a -> SmallArray# a
insertSmallArray# i a arr = insertSmallArray_ i (unsafeCoerce# a) arr
{-# inline insertSmallArray# #-}
