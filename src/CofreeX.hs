{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE TypeOperators, TypeApplications, RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module CofreeX () where

import FreeX

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

era :: [Int] -> StreamF Int [Int]
era (p : ns) = SConsF p (filter ((/= 0) . (`mod` p)) ns)

primeStream = ana era [2..]

toListC :: Fix (StreamF e) -> [e]
toListC (In (SConsF x s)) = x : toListC s
{-
toListC = cata alg
  where alg :: (StreamF e a) -> a
        alg (SConsF e s) = e : s
-}

primes = toListC primeStream

class Comonoid w where
  splitC :: w -> (w, w)
  destroy :: w -> ()

data Prod e a = Prod e a

instance Functor (Prod e) where
  fmap f (Prod e a) = Prod e (f a)

instance Comonad (Prod e) where
  extract (Prod e a) = a
  duplicate (Prod e a) = Prod e (Prod e a)

fprod (Prod e a) = e + a
gprod (Prod e a) = e * a

data List a = Nil | ConsL a (List a)

instance Functor List where
  fmap f Nil = Nil
  fmap f (ConsL a l) = ConsL (f a) $ fmap f l

instance Applicative List where
  pure a = ConsL a Nil
  Nil <*> _ = Nil
  ConsL f fs <*> as = concatL (fmap f as) (fs <*> as)

instance Monad List where
  return a = ConsL a Nil
  Nil >>= f = Nil
  (ConsL a l) >>= f = concatL (f a) $ l >>= f

join Nil = Nil
join (ConsL as tls) = concatL as $ join tls

concatL :: List a -> List a -> List a
concatL Nil rs = rs
concatL (ConsL a l) rs = ConsL a $ concatL l rs

