module SlideWindow where

import           Data.List
import qualified Data.Vector           as V
import qualified Data.Vector.Mutable   as MV
import           Control.Monad.ST

slideWindowSimple :: Int -> ([a] -> b) -> [a] -> [b]
slideWindowSimple n f xs
  | length xs < n = []
  | otherwise     = go (length xs) xs
  where go l ts
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

slideWindowSemigrp :: Semigroup a => Int -> [a] -> [a]
slideWindowSemigrp n xs
  | length xs < n = []
  | otherwise     = go (drop n xs) $ scanr1 (<>) (take n xs)
  where go []     (b:bs) = [b]
        go (t:ts) (b:bs) = b : go ts (map (<> t) bs ++ [t])

-- Slide window in vector
--
slideWindowVSimple :: Int -> (V.Vector a -> b) -> V.Vector a -> V.Vector b
slideWindowVSimple n f vec
  | length vec < n = V.empty
  | otherwise      = runST $ do mvec <- (MV.new (length vec - n + 1))
                                go 0 mvec vec 
  where go i mvec tvec
          | V.length tvec < n = V.unsafeFreeze mvec
          | otherwise         = do
                              MV.write mvec i $ f (V.take n tvec)
                              go (i+1) mvec (V.tail tvec)

slideWindowVAccuR :: Int -> (a -> b -> b) -> b -> V.Vector a -> V.Vector b
slideWindowVAccuR n f z vec
  | length vec < n = V.empty
  | otherwise      = runST $ do mvec <- (MV.new (length vec - n + 1))
                                go 0 mvec (V.drop n vec) (V.scanr f z $ V.take n vec)
  where go i mvec tvec bvec
          | V.null tvec = do
                        MV.write mvec i b
                        V.unsafeFreeze mvec
          | otherwise   = do
                        MV.write mvec i b
                        go (i+1) mvec ts $ V.map (f t) bs `V.snoc` z
          where (t, ts) = (V.head tvec, V.tail tvec)
                (b, bs) = (V.head bvec, V.tail bvec)

slideWindowVSemigrp :: Semigroup a => Int -> V.Vector a -> V.Vector a
slideWindowVSemigrp n vec
  | length vec < n = V.empty
  | otherwise      = runST $ do mvec <- (MV.new (length vec - n + 1))
                                go 0 mvec (V.drop n vec) (V.scanr1 (<>) $ V.take n vec)
  where go i mvec tvec bvec
          | V.null tvec = do
                        MV.write mvec i b
                        V.unsafeFreeze mvec
          | otherwise   = do
                        MV.write mvec i b
                        go (i+1) mvec ts $ V.map (<> t) bs `V.snoc` t
          where (t, ts) = (V.head tvec, V.tail tvec)
                (b, bs) = (V.head bvec, V.tail bvec)
