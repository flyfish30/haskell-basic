{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE TypeOperators, TypeApplications, RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module CofreeX () where

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate
  (=>>) :: w a -> (w a -> b) -> w b
  (=>>) = flip extend

data Stream a = a :< Stream a deriving Show
instance Functor Stream where
  fmap f (x :< xs) = f x :< fmap f xs
instance Comonad Stream where
  extract (x :< _) = x
  duplicate s@(_:<xs) = s :< duplicate xs

generate :: (a -> a) -> a -> Stream a
generate f x = x :< generate f x =>> (f . extract)

nats :: Stream Integer
nats = generate (+1) 0

getWindowSum :: Integer -> Stream Integer -> Integer
getWindowSum n (x :< xs)
  | n == 1 = x
  | n >  1 = x + getWindowSum (n-1) xs
  | otherwise = undefined

tensWindows = nats =>> getWindowSum 10

data Store s a = Store (s -> a) s

instance Functor (Store s) where
  fmap f (Store g s) = Store (f . g) s

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) = Store (Store f) s

-- Define Cofree comonad
data Cofree f a = Cofree a (f (Cofree f a))

instance Functor f => Functor (Cofree f) where
  fmap f (Cofree a fcf) = Cofree (f a) (fmap (fmap f) fcf)

instance Functor f => Comonad (Cofree f) where
  extract (Cofree a fcf) = a
  duplicate cof@(Cofree a fcf) = Cofree cof (fmap duplicate fcf)

-- F-Alg and Cofree Comonad
type Coalg f a = a -> f a

newtype Fix f = In { out :: f (Fix f) }
instance Show (f (Fix f)) => Show (Fix f) where
  show (In fix) = "(In (" ++ show fix ++ "))"

ana :: Functor f => Coalg f a -> a -> Fix f
ana coalg = In . fmap (ana coalg) . coalg

-- Cofree comonoid is Stream a
type CofreeComon a = Stream a

-- data Stream a = Cons a (Stream a)
--               deriving Show

data StreamF a x = SConsF a x
                 deriving Show

instance Functor (StreamF a) where
  fmap f (SConsF a x) = SConsF a (f x)

data ListF a x = NilF
               | ConsF a x

instance Functor (ListF a) where
  fmap f NilF = NilF
  fmap f (ConsF a x) = ConsF a (f x)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr coalg b = case coalg b of
  Nothing -> []
  Just (a, b') -> a : unfoldr coalg b'

genNat = unfoldr go 0
  where go b = Just (b, b + 1)

data Tape a = Tape { tLeft  :: [a]
                   , tVal   :: a
                   , tRight :: [a]
                   }
shiftLeft :: Tape a -> Tape a
shiftLeft (Tape ls v (r:rs)) = Tape (v:ls) r rs

shiftLeftN :: Int -> Tape a -> Tape a
shiftLeftN n t = iterate shiftLeft t !! n

shiftRight :: Tape a -> Tape a
shiftRight (Tape (l:ls) v rs) = Tape ls l (v:rs)

instance Functor Tape where
  fmap f (Tape ls v rs) = Tape (fmap f ls) (f v) (fmap f rs)

instance Comonad Tape where
  extract (Tape ls v rs) = v
  duplicate t@(Tape ls v rs) = Tape ls' t rs'
    where (_:ls') = iterate shiftRight t 
          (_:rs') = iterate shiftLeft t
