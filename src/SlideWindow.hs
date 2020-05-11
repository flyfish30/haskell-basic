{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}

module SlideWindow where

import           Data.List
import           Data.Foldable
import           Data.Group
import qualified Data.Vector.Unboxed           as V
import qualified Data.Vector.Unboxed.Mutable   as MV
import           Control.Monad.ST

slideWindowSimple :: Int -> ([a] -> b) -> [a] -> [b]
slideWindowSimple n f xs
  | length xs < n = []
  | otherwise     = go (length xs) xs
  where go !l !ts
          | l < n     = []
          | otherwise = f (take n ts) : go (l-1) (tail ts)

slideWindowAccuR :: Int -> (a -> b -> b) -> b ->  [a] -> [b]
slideWindowAccuR n f z xs
  | length xs < n = []
  | otherwise     = go (drop n xs) $ scanr f z (take n xs)
  where go []     (b:bs) = [b]
        go (t:ts) (b:bs) = b : go ts (map (f t) bs ++ [z])

instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty  = 0
  mappend = (<>)

instance Group Int where
  invert = negate
  pow x n = x * fromIntegral n

slideWindowSemigrp :: Semigroup a => Int -> [a] -> [a]
slideWindowSemigrp n xs
  | length xs < n = []
  | otherwise     = go (drop n xs) $ scanr1 (<>) (take n xs)
  where go []     (b:bs) = [b]
        go (t:ts) (b:bs) = b : go ts (map (<> t) bs ++ [t])

slideWindowGroup :: Group a => Int -> [a] -> [a]
slideWindowGroup n xs
  | length xs < n = []
  | otherwise     = go (length xs) xs $ foldr1 (<>) (take n xs)
  where go !l (t:ts) !b
          | l < n + 1 = [b]
          | otherwise = b : go (l-1) ts (b <> invert t <> ts !! (n - 1))

slideWindowInt :: Int -> [Int] -> [Int]
slideWindowInt n xs
  | length xs < n = []
  | otherwise     = go (length xs) xs $ sum (take n xs)
  where go !l (t:ts) !b
          | l < n + 1 = [b]
          | otherwise = b : go (l-1) ts (b - t + ts !! (n - 1))

-- Slide window on vector
--
mapModify f vec = go (MV.length vec - 1)
  where go 0 = MV.modify vec f 0
        go n = do MV.modify vec f n
                  go (n - 1)

shiftMapInAtN f vec = go 0 (MV.length vec - 2)
  where go n l
          | n < l     = do a <- MV.read vec (n + 1)
                           MV.write vec n $ f a
                           go (n + 1) l
          | otherwise = do a <- MV.read vec (n + 1)
                           MV.write vec n $ f a

slideWindowVSimple :: (MV.Unbox a, MV.Unbox b)
                   => Int -> (V.Vector a -> b) -> V.Vector a -> V.Vector b
-- slideWindowVSimple :: Int -> (V.Vector Int -> Int) -> V.Vector Int -> V.Vector Int
slideWindowVSimple n f vec
  | V.length vec < n = V.empty
  | otherwise        = runST $ do mvec <- (MV.new (V.length vec - n + 1))
                                  go 0 mvec vec
  where go !i mvec !tvec
          | V.length tvec < n = V.unsafeFreeze mvec
          | otherwise         = do
                              MV.write mvec i $ f (V.take n tvec)
                              go (i+1) mvec (V.tail tvec)

slideWindowVAccuR :: (MV.Unbox a, MV.Unbox b)
                  => Int -> (a -> b -> b) -> b -> V.Vector a -> V.Vector b
slideWindowVAccuR n f z vec
  | V.length vec < n = V.empty
  | otherwise        = runST $ do mvec <- (MV.new (V.length vec - n + 1))
                                  go 0 mvec (V.drop n vec) (V.scanr f z $ V.take n vec)
  where go !i mvec !tvec !bvec
          | V.null tvec = do
                        MV.write mvec i b
                        V.unsafeFreeze mvec
          | otherwise   = do
                        MV.write mvec i b
                        go (i+1) mvec ts $ V.map (f t) bs `V.snoc` z
          where (t, ts) = (V.head tvec, V.tail tvec)
                (b, bs) = (V.head bvec, V.tail bvec)

slideWindowVSemigrp :: (Semigroup a, MV.Unbox a)
                    => Int -> V.Vector a -> V.Vector a
-- slideWindowVSemigrp :: Int -> V.Vector Int -> V.Vector Int
slideWindowVSemigrp n vec
  | V.length vec < n = V.empty
  | otherwise        = runST $ do mvec <- (MV.new (V.length vec - n + 1))
                                  bvec <- V.thaw $ V.scanr1 (<>) $ V.take n vec
                                  go 0 mvec (V.drop n vec) bvec
  where go !i mvec !tvec !bvec
          | V.null tvec = do
                        b <- MV.read bvec 0
                        MV.write mvec i b
                        V.unsafeFreeze mvec
          | otherwise   = do
                        b <- MV.read bvec 0
                        MV.write mvec i b
                        shiftMapInAtN (<> t) bvec
                        MV.write bvec (MV.length bvec - 1) t
                        go (i+1) mvec ts bvec
          where (t, ts) = (V.head tvec, V.tail tvec)

slideWindowVGroup :: (Group a, MV.Unbox a)
                  => Int -> V.Vector a -> V.Vector a
-- slideWindowVGroup :: Int -> V.Vector Int -> V.Vector Int
slideWindowVGroup n vec
  | len < n   = V.empty
  | otherwise = runST $ do mvec <- (MV.new (len - n + 1))
                           go 0 mvec vec (V.foldr1 (<>) $ V.take n vec)
  where len = V.length vec
        go !i mvec !tvec !b
          | i < len - n = do
                        MV.write mvec i b
                        go (i+1) mvec ts (b <> invert t <> ts V.! (n - 1))
          | otherwise   = do
                        MV.write mvec i b
                        V.unsafeFreeze mvec
          where (t, ts) = (V.head tvec, V.tail tvec)

-- slideWindowVInt :: (MV.Unbox a, Num a) => Int -> V.Vector a -> V.Vector a
slideWindowVInt :: Int -> V.Vector Int -> V.Vector Int
slideWindowVInt n vec
  | len < n   = V.empty
  | otherwise = runST $ do mvec <- (MV.new (len - n + 1))
                           go 0 mvec vec (V.foldr1 (+) $ V.take n vec)
  where len = V.length vec
        go !i mvec !tvec !b
          | i < len - n = do
                        MV.write mvec i b
                        go (i+1) mvec ts (b - t + ts V.! (n - 1))
          | otherwise   = do
                        MV.write mvec i b
                        V.unsafeFreeze mvec
          where (t, ts) = (V.head tvec, V.tail tvec)
