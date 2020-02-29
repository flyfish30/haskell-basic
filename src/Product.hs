{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Product where

-- | Product a b
type a * b = (a, b)

outl :: (b1 * b2) -> b1
-- outl = fst
outl (x, y) = x

outr :: (b1 * b2) -> b2
-- outr = snd
outr (x, y) = y

(/\) :: (a -> b1) -> (a -> b2) -> a -> (b1 * b2)
f1 /\ f2 = \x -> (f1 x, f2 x)

-- | Coproduct a b
type a + b = Either a b

inl :: b1 -> (b1 + b2)
inl x = Left x

inr :: b2 -> (b1 + b2)
inr y = Right y

(\/) :: (b1 -> a) -> (b2 -> a) -> (b1 + b2) -> a
(g1 \/ g2) (Left x)  = g1 x
(g1 \/ g2) (Right y) = g2 y

class Bifunctor p where
  bimap :: (a1 -> b1) -> (a2 -> b2) -> p a1 a2 -> p b1 b2

instance Bifunctor (,) where
  bimap f1 f2 = \(x, y) -> (f1 x, f2 y)

instance Bifunctor Either where
  bimap f1 f2 (Left  l) = Left  (f1 l)
  bimap f1 f2 (Right r) = Right (f2 r)

assoc  ((a, b), c) = (a, (b, c))
cossoc (a, (b, c)) = ((a, b), c)

-- ($) is id at function
apply = uncurry ($)

mu :: (a -> a, a -> a) -> a -> a
mu = curry (apply . bimap ($) apply . assoc)

-- | Product functor f and g
data Product f g a = Pair (f a) (g a)
                     deriving Show
infixr 6 :*:
type (:*:) = Product

-- | Coproduct functor f and g
data Coproduct f g a = InL (f a) | InR (g a)
                     deriving Show
infixr 5 :+:
type (:+:) = Coproduct

