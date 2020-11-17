{-# LANGUAGE 
  FlexibleInstances, 
  UndecidableInstances, 
  InstanceSigs,
  ScopedTypeVariables,
  RankNTypes #-}

module PeanoChurch where

import Data.List
import Data.Profunctor

type ISO a b = (a -> b, b -> a)
-- See https://www.codewars.com/kata/isomorphism

symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd

liftISO :: ISO a b -> ISO (a -> a) (b -> b)
liftISO (ab, ba) = (dimap ba ab, dimap ab ba)
 
liftISO2 :: ISO a b -> ISO (a -> a -> a) (b -> b -> b)
liftISO2 (ab, ba) = (dimap ba (dimap ba ab), dimap ab (dimap ab ba))

-- A Natural Number is either Zero,
-- or a Successor (1 +) of Natural Number.
-- We have (+)/(*) on Natural Number, or (-) it.
-- Since Natrual Number do not have negative, forall x, 0 - x = 0.
-- We also have pow on Natrual Number
-- Since Haskell is lazy, we also have infinity

class Nat n where
  zero :: n
  successor :: n -> n
  nat :: a -> (n -> a) -> n -> a -- Pattern Matching
  iter :: a -> (a -> a) -> n -> a -- Induction
  plus, minus, mult, pow :: n -> n -> n
  inf :: n
  inf = successor inf
  divide :: n -> n -> n
  l `divide` r | l == 0 && r == 0 = undefined
  l `divide` r | l < r = 0
  l `divide` r | otherwise = successor $ (l `minus` r) `divide` r
  -- notice (l `divide` 0) when l is not 0 will return inf
  isoP :: ISO n Peano -- See below for the definition of Peano
  isoP = (iter zero successor, iter zero successor)
  toP :: n -> Peano
  toP = substL isoP

instance {-# OVERLAPPABLE #-} Nat n => Show n where
  show = show . toP

instance {-# OVERLAPPABLE #-} Nat n => Eq n where
  l == r = toP l == toP r

instance {-# OVERLAPPABLE #-} Nat n => Ord n where
  l `compare` r = toP l `compare` toP r

instance {-# OVERLAPPABLE #-} Nat n => Num n where
  abs = id
  signum = nat zero (const 1)
  fromInteger 0 = zero
  fromInteger n | n > 0 = successor $ fromInteger (n - 1)
  (+) = plus
  (*) = mult
  (-) = minus

-- We can encode Natrual Number directly as Algebraic Data Type(ADT).
data Peano = O | S Peano deriving (Show, Eq, Ord)

-- Remember, 0 - x = 0 for all x.
instance Nat Peano where
  zero      = O
  successor = S
  nat  z s O      = z
  nat  z s (S pn) = s pn
  iter z step O      = z
  iter z step (S pn) = step $ iter z step pn

  plus  m n = iter m successor n
  minus m n
    | m > n = iter m predPeano n
    | otherwise = 0
  mult  O n = 0
  mult  m n = iter 0 (plus m) n
  pow   O n = 1
  pow   m n = iter 1 (mult m) n

  isoP = (id, id)
  toP  = id

predPeano :: Peano -> Peano
predPeano O = error "Zero peano hasn't predecessor"
predPeano (S pn) = pn

-- Peano is very similar to a basic data type in Haskell - List!
-- O is like [], and S is like :: (except it lack the head part)
-- When we want to store no information, we can use (), a empty tuple
-- This is different from storing nothing (called Void in Haskell),
-- as we can create a value of () by using (), 
-- but we cannot create a value of Void.

-- Notice how you can implement everything once you have isoP,
-- By converting to Peano and using Nat Peano?
-- Dont do that. You wont learn anything.
-- Try to use operation specific to list.
instance Nat [()] where
  zero      = []
  successor = (() :)
  nat z s []     = z
  nat z s (_:xs) = s xs
  iter z step []     = z
  iter z step (_:xs) = step $ iter z step xs

  plus  m  n = iter m successor n
  minus m  n
    | m > n  = iter m predList n
    | otherwise = 0
  mult  [] n = 0
  mult  m  n = iter 0 (plus m) n
  pow   [] n = 1
  pow   m  n = iter 1 (mult m) n

  isoP = (iter O successor, iter [] successor)
  toP  = iter O successor

predList :: [()] -> [()]
predList [] = error "Empty list hasn't predecessor"
predList (_:xs) = xs

-- Instead of defining Nat from zero, sucessor (and get Peano),
-- We can define it from Pattern Matching
newtype Scott = Scott { runScott :: forall a. a -> (Scott -> a) -> a }
instance Nat Scott where
  -- Other operation on Scott numeral is sort of boring,
  -- So we implement it using operation on Peano.
  -- You shouldnt do this - I had handled all the boring case for you.
  plus = substR (liftISO2 isoP) plus
  minus = substR (liftISO2 isoP) minus
  mult = substR (liftISO2 isoP) mult
  pow = substR (liftISO2 isoP) pow

  zero = Scott $ \z s -> z
  successor n = Scott $ \z s -> s n
  nat z s n = runScott n z s
  iter z step n = runScott n z (step . iter z step)

-- Or from induction!
newtype Church = Church { runChurch :: forall a. (a -> a) -> a -> a }
instance Nat Church where
  -- Try to implement the calculation (except minus) in the primitive way.
  -- Implement them by constructing Church explicitly.
  -- So plus should not use successor,
  -- mult should not use plus,
  -- exp should not use mult.
  zero        = Church $ const id
  successor n = Church $ \f -> f . (runChurch n f)
  nat z s n   = runChurch n (const $ s (predChurch n)) z
  iter z step n = runChurch n step z

  plus  m n = Church $ \f -> (runChurch n f) . (runChurch m f)
  minus m n
    | m > n = runChurch n predChurch m
    | otherwise = 0
  mult  0 n = 0
  mult  m n = Church $ runChurch n . runChurch m
  pow   m n = Church $ runChurch n $ runChurch m

  isoP = (\n -> runChurch n successor 0, iter 0 successor)
  toP n = runChurch n successor 0

predChurch :: Church -> Church
predChurch 0 = error "Zero church hasn't predecessor"
predChurch n = Church $ \f x -> runChurch n (\g h -> h (g f)) (const x) id
