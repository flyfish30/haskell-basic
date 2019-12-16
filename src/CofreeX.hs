{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE TypeOperators, TypeApplications, RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module FreeX () where

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

