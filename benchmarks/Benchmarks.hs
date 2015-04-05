
{-# LANGUAGE
    ScopedTypeVariables, RankNTypes, BangPatterns,
    BangPatterns, CPP, MagicHash, UnboxedTuples,
    UnliftedFFITypes, GHCForeignImportPrim #-}

import GHC.Types
import GHC.Prim
import GHC.ST

import GHC.Prim.Array
import GHC.Prim.SmallArray
import Criterion.Main
import Criterion.Types


an  n = fromListA  [(0::Int)..n]
san n = fromListSA [(0::Int)..n]

a10   = an 10
a100  = an 100
a1000 = an 1000

sa10   = san 10
sa100  = san 100
sa1000 = san 1000


main = defaultMainWith (defaultConfig {timeLimit = 2}) [
  bgroup "cons" [
     
     bgroup "Array" [
        bgroup "10" [
           bench "prim" $ whnf (consA 0)     a10,
           bench "slow" $ whnf (consASlow 0) a10
           ],
        bgroup "100" [
           bench "prim" $ whnf (consA 0)     a100,
           bench "slow" $ whnf (consASlow 0) a100
           ],
        bgroup "1000" [
           bench "prim" $ whnf (consA 0)     a1000,
           bench "slow" $ whnf (consASlow 0) a1000
           ]
        ],
        
     bgroup "SmallArray" [
        bgroup "10" [
           bench "prim" $ whnf (consSA 0)     sa10,
           bench "slow" $ whnf (consSASlow 0) sa10
           ],
        bgroup "100" [
           bench "prim" $ whnf (consSA 0)     sa100,
           bench "slow" $ whnf (consSASlow 0) sa100
           ],
        bgroup "1000" [
           bench "prim" $ whnf (consSA 0)     sa1000,
           bench "slow" $ whnf (consSASlow 0) sa1000
           ]
        ]
     ],
  
  bgroup "snoc" [
     
     bgroup "Array" [
        bgroup "10" [
           bench "prim" $ whnf (flip snocA 0)     a10,
           bench "slow" $ whnf (flip snocASlow 0) a10
           ],
        bgroup "100" [
           bench "prim" $ whnf (flip snocA 0)     a100,
           bench "slow" $ whnf (flip snocASlow 0) a100
           ],
        bgroup "1000" [
           bench "prim" $ whnf (flip snocA 0)     a1000,
           bench "slow" $ whnf (flip snocASlow 0) a1000
           ]
        ],
        
     bgroup "SmallArray" [
        bgroup "10" [
           bench "prim" $ whnf (flip snocSA 0)     sa10,
           bench "slow" $ whnf (flip snocSASlow 0) sa10
           ],
        bgroup "100" [
           bench "prim" $ whnf (flip snocSA 0)     sa100,
           bench "slow" $ whnf (flip snocSASlow 0) sa100
           ],
        bgroup "1000" [
           bench "prim" $ whnf (flip snocSA 0)     sa1000,
           bench "slow" $ whnf (flip snocSASlow 0) sa1000
           ]
        ]
     ],

  bgroup "insert" [
     
     bgroup "Array" [
        bgroup "10" [
           bench "prim" $ whnf (insertA 5 10)     a10,
           bench "slow" $ whnf (insertASlow 5 10) a10
           ],
        bgroup "100" [
           bench "prim" $ whnf (insertA 50 10)     a100,
           bench "slow" $ whnf (insertASlow 50 10) a100
           ],
        bgroup "1000" [
           bench "prim" $ whnf (insertA 500 10)     a1000,
           bench "slow" $ whnf (insertASlow 500 10) a1000
           ]
        ],
        
     bgroup "SmallArray" [
        bgroup "10" [
           bench "prim" $ whnf (insertSA 5 10)     sa10,
           bench "slow" $ whnf (insertSASlow 5 10) sa10
           ],
        bgroup "100" [
           bench "prim" $ whnf (insertSA 50 10)     sa100,
           bench "slow" $ whnf (insertSASlow 50 10) sa100
           ],
        bgroup "1000" [
           bench "prim" $ whnf (insertSA 500 10)     sa1000,
           bench "slow" $ whnf (insertSASlow 500 10) sa1000
           ]
        ]
     ],
  
  bgroup "delete" [
     
     bgroup "Array" [
        bgroup "10" [
           bench "prim" $ whnf (deleteA 5)     a10,
           bench "slow" $ whnf (deleteASlow 5) a10
           ],
        bgroup "100" [
           bench "prim" $ whnf (deleteA 50)     a100,
           bench "slow" $ whnf (deleteASlow 50) a100
           ],
        bgroup "1000" [
           bench "prim" $ whnf (deleteA 500)     a1000,
           bench "slow" $ whnf (deleteASlow 500) a1000
           ]
        ],
        
     bgroup "SmallArray" [
        bgroup "10" [
           bench "prim" $ whnf (deleteSA 5)     sa10,
           bench "slow" $ whnf (deleteSASlow 5) sa10
           ],
        bgroup "100" [
           bench "prim" $ whnf (deleteSA 50)     sa100,
           bench "slow" $ whnf (deleteSASlow 50) sa100
           ],
        bgroup "1000" [
           bench "prim" $ whnf (deleteSA 500)     sa1000,
           bench "slow" $ whnf (deleteSASlow 500) sa1000
           ]
        ]
     ]
  ]


-- Array utility

data A a = A (Array# a)

toListA :: forall a. A a -> [a]
toListA (A arr) = go arr 0# where
    go arr i | isTrue# (i ==# sizeofArray# arr) = []
    go arr i = case indexArray# arr i of
        (# a #) -> a : go arr (i +# 1#)

fromListA :: [a] -> A a
fromListA xs = runSTRep $ \s -> let !(I# size) = length xs in
    case newArray# size undefined s of
        (# s, marr #) -> go xs 0# s where
            go (x:xs) i s = case writeArray# marr i x s of s -> go xs (i +# 1#) s
            go _      _ s = case unsafeFreezeArray# marr s of
                (# s , arr #) -> (# s, A arr #)

consA :: a -> A a -> A a
consA a (A arr) = A (consArray# (unsafeCoerce# a) arr)

consASlow :: a -> A a -> A a
consASlow a (A arr) = runSTRep $ \s ->
  let !size = sizeofArray# arr in
  case newArray# (size +# 1#) undefined s of
    (# s, marr #) -> case writeArray# marr 0# a s of
      s -> case copyArray# arr 0# marr 1# size s of
        s -> case unsafeFreezeArray# marr s of
          (# s, arr #) -> (# s, A arr #)

snocA :: A a -> a -> A a
snocA (A arr) a = A (snocArray# arr (unsafeCoerce# a))

snocASlow :: A a -> a -> A a
snocASlow (A arr) a = runSTRep $ \s ->
  let !size = sizeofArray# arr in
  case newArray# (size +# 1#) undefined s of
    (# s, marr #) -> case writeArray# marr size a s of
      s -> case copyArray# arr 0# marr 0# size s of
        s -> case unsafeFreezeArray# marr s of
          (# s, arr #) -> (# s, A arr #)

insertA :: Int -> a -> A a -> A a
insertA (I# i) a (A arr) = A (insertArray# i (unsafeCoerce# a) arr)

insertASlow :: Int -> a -> A a -> A a
insertASlow (I# i) a (A arr) = runSTRep $ \s ->
  let !size = sizeofArray# arr in
  case newArray# (size +# 1#) undefined s of
    (# s, marr #) -> case writeArray# marr i a s of
      s -> case copyArray# arr 0# marr 0# i s of
        s -> case copyArray# arr i marr (i +# 1#) (size -# i) s of
          s -> case unsafeFreezeArray# marr s of
            (# s, arr #) -> (# s, A arr #)

deleteA :: Int -> A a -> A a
deleteA (I# i) (A arr) = A (deleteArray# i arr)

deleteASlow :: Int -> A a -> A a
deleteASlow (I# i) (A arr) = runSTRep $ \s ->
  let !size = sizeofArray# arr in
  case newArray# (size -# 1#) undefined s of
    (# s, marr #) -> case copyArray# arr 0# marr 0# i s of
      s -> case copyArray# arr (i +# 1#) marr i (size -# i -# 1#) s of
        s -> case unsafeFreezeArray# marr s of
          (# s, arr #) -> (# s, A arr #)


-- SmallArray utility

data SA a = SA (SmallArray# a)

toListSA :: forall a. SA a -> [a]
toListSA (SA arr) = go arr 0# where
    go arr i | isTrue# (i ==# sizeofSmallArray# arr) = []
    go arr i = case indexSmallArray# arr i of
        (# a #) -> a : go arr (i +# 1#)

fromListSA :: [a] -> SA a
fromListSA xs = runSTRep $ \s -> let !(I# size) = length xs in
    case newSmallArray# size undefined s of
        (# s, marr #) -> go xs 0# s where
            go (x:xs) i s = case writeSmallArray# marr i x s of s -> go xs (i +# 1#) s
            go _      _ s = case unsafeFreezeSmallArray# marr s of
                (# s , arr #) -> (# s, SA arr #)

consSA :: a -> SA a -> SA a
consSA a (SA arr) = SA (consSmallArray# (unsafeCoerce# a) arr)

consSASlow :: a -> SA a -> SA a
consSASlow a (SA arr) = runSTRep $ \s ->
  let !size = sizeofSmallArray# arr in
  case newSmallArray# (size +# 1#) undefined s of
    (# s, marr #) -> case writeSmallArray# marr 0# a s of
      s -> case copySmallArray# arr 0# marr 1# size s of
        s -> case unsafeFreezeSmallArray# marr s of
          (# s, arr #) -> (# s, SA arr #)

snocSA :: SA a -> a -> SA a
snocSA (SA arr) a = SA (snocSmallArray# arr (unsafeCoerce# a))

snocSASlow :: SA a -> a -> SA a
snocSASlow (SA arr) a = runSTRep $ \s ->
  let !size = sizeofSmallArray# arr in
  case newSmallArray# (size +# 1#) undefined s of
    (# s, marr #) -> case writeSmallArray# marr size a s of
      s -> case copySmallArray# arr 0# marr 0# size s of
        s -> case unsafeFreezeSmallArray# marr s of
          (# s, arr #) -> (# s, SA arr #)

insertSA :: Int -> a -> SA a -> SA a
insertSA (I# i) a (SA arr) = SA (insertSmallArray# i (unsafeCoerce# a) arr)

insertSASlow :: Int -> a -> SA a -> SA a
insertSASlow (I# i) a (SA arr) = runSTRep $ \s ->
  let !size = sizeofSmallArray# arr in
  case newSmallArray# (size +# 1#) undefined s of
    (# s, marr #) -> case writeSmallArray# marr i a s of
      s -> case copySmallArray# arr 0# marr 0# i s of
        s -> case copySmallArray# arr i marr (i +# 1#) (size -# i) s of
          s -> case unsafeFreezeSmallArray# marr s of
            (# s, arr #) -> (# s, SA arr #)
            
deleteSA :: Int -> SA a -> SA a
deleteSA (I# i) (SA arr) = SA (deleteSmallArray# i arr)

deleteSASlow :: Int -> SA a -> SA a
deleteSASlow (I# i) (SA arr) = runSTRep $ \s ->
  let !size = sizeofSmallArray# arr in
  case newSmallArray# (size -# 1#) undefined s of
    (# s, marr #) -> case copySmallArray# arr 0# marr 0# i s of
      s -> case copySmallArray# arr (i +# 1#) marr i (size -# i -# 1#) s of
        s -> case unsafeFreezeSmallArray# marr s of
          (# s, arr #) -> (# s, SA arr #)
                          
