module SlideWindow where

import Data.List

slideWindowSimple :: Int -> ([a] -> b) -> [a] -> [b]
slideWindowSimple n f xs
  | length xs < n = []
  | otherwise     = f (take n xs) : slideWindowSimple n f (tail xs)

slideWindowScanr :: Int -> (a -> b -> b) -> b ->  [a] -> [b]
slideWindowScanr n f z xs
  | length xs < n = []
  | otherwise     = go (drop n xs) $ scanr f z (take n xs)
  where go []     (b:bs) = [b]
        go (t:ts) (b:bs) = b : go ts (map (f t) bs ++ [z])

instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty  = 0
  mappend = (+)

slideWindowMonoid :: Monoid a => Int -> [a] -> [a]
slideWindowMonoid n xs
  | length xs < n = []
  | otherwise     = go (drop n xs) $ scanr1 mappend (take n xs)
  where go []     (b:bs) = [b]
        go (t:ts) (b:bs) = b : go ts (map (`mappend` t) (bs ++ [mempty]))
