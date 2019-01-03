{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
g1 \/ g2 = \x -> case x of
                   (Left  l) -> g1 l
                   (Right r) -> g2 r

class Bifunctor p where
  bimap :: (a1 -> b1) -> (a2 -> b2) -> p a1 a2 -> p b1 b2

instance Bifunctor (,) where
  bimap f1 f2 = \(x, y) -> (f1 x, f2 y)

instance Bifunctor Either where
  bimap f1 f2 (Left  l) = Left  (f1 l)
  bimap f1 f2 (Right r) = Right (f2 r)

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
