
{-# LANGUAGE
    MagicHash, ForeignFunctionInterface,
    GHCForeignImportPrim, UnliftedFFITypes #-}

module GHC.Prim.Array (
    consArray#
  , snocArray#
  , insertArray#
  , deleteArray#
  ) where

import GHC.Prim

-- | Prepend an element to an array.
foreign import prim "consArray" consArray_
  :: Any -> Array# a -> Array# a

-- | Append an element to the end of an array.
foreign import prim "snocArray" snocArray_
  :: Array# a -> Any -> Array# a

-- | Insert new element at position.
foreign import prim "insertArray" insertArray_
  :: Int# -> Any -> Array# a -> Array# a

-- | Delete element at position. 
foreign import prim "deleteArray" deleteArray#
  :: Int# -> Array# a -> Array# a

consArray# :: a -> Array# a -> Array# a
consArray# a arr = consArray_ (unsafeCoerce# a) arr
{-# inline consArray# #-}

snocArray# :: Array# a -> a -> Array# a
snocArray# arr a = snocArray_ arr (unsafeCoerce# a)
{-# inline snocArray# #-}

insertArray# :: Int# -> a -> Array# a -> Array# a
insertArray# i a arr = insertArray_ i (unsafeCoerce# a) arr
{-# inline insertArray# #-}

