{-# language RankNTypes, MagicHash, UnboxedTuples, BangPatterns,
    ScopedTypeVariables #-}

import GHC.Prim
import GHC.Types
import GHC.Prim.Array
import GHC.Prim.SmallArray

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

runSTRep :: (forall s. State# s -> (# State# s, a #)) -> a
runSTRep f = case f realWorld# of (# _ , a #) -> a
{-# INLINE [0] runSTRep #-}

insList :: Int -> a -> [a] -> [a]
insList n a as = let (as1, as2) = splitAt n as in as1 ++ a:as2

delList :: Int -> [a] -> [a]
delList n as = case splitAt n as of
  (as1, _:as2) -> as1 ++ as2
  _            -> as

data A a = A (Array# a)

consA a (A arr) = A (consArray# a arr)
snocA (A arr) a = A (snocArray# arr a)
insertA (I# i) a (A arr) = A (insertArray# i a arr)
deleteA (I# i) (A arr)   = A (deleteArray# i arr)

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

data SA a = SA (SmallArray# a)

consSA a (SA arr) = SA (consSmallArray# a arr)
snocSA (SA arr) a = SA (snocSmallArray# arr a)
insertSA (I# i) a (SA arr) = SA (insertSmallArray# i a arr)
deleteSA (I# i) (SA arr)   = SA (deleteSmallArray# i arr)

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

main :: IO ()
main = defaultMain $ testGroup "tests" [
  testGroup "smallArray" [
      testProperty "cons" $ \(xs :: [Int]) x ->
        (x:xs) == toListSA (consSA x (fromListSA xs))

    , testProperty "snoc" $ \(xs :: [Int]) x ->
        (xs ++ [x]) == toListSA (snocSA (fromListSA xs) x)

    , testProperty "insert" $ \(xs :: [Int]) x ->
        forAll (choose (0, length xs)) $ \n ->
          insList n x xs == toListSA (insertSA n x (fromListSA xs))

    , testProperty "delete" $ \(NonEmpty (xs :: [Int])) ->
        forAll (choose (0, length xs - 1)) $ \n ->
          delList n xs == toListSA (deleteSA n (fromListSA xs))
    ],

  testGroup "array" [
      testProperty "cons" $ \(xs :: [Int]) x ->
        (x:xs) == toListA (consA x (fromListA xs))

    , testProperty "snoc" $ \(xs :: [Int]) x ->
        (xs ++ [x]) == toListA (snocA (fromListA xs) x)

    , testProperty "insert" $ \(xs :: [Int]) x ->
        forAll (choose (0, length xs)) $ \n ->
          insList n x xs == toListA (insertA n x (fromListA xs))

    , testProperty "delete" $ \(NonEmpty (xs :: [Int])) ->
        forAll (choose (0, length xs - 1)) $ \n ->
          delList n xs == toListA (deleteA n (fromListA xs))
    ]
  ]



