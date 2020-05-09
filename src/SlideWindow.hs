module SlideWindow where

import           Data.List
import qualified Data.Vector           as V
import qualified Data.Vector.Mutable   as MV

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
