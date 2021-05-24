{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}

module FunctorAlg where

-- | Product a b
infixr 7 *
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
infixr 6 +
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

data Const mo a = Const mo
                deriving Show

instance Functor (Const mo) where
  fmap _ (Const mo) = Const mo

instance Monoid mo => Applicative (Const mo) where
  pure = Const mempty
  Const mf <*> Const mo = Const (mf <> mo)

-- | Definition Natural and natural constructs
class Natural f g where
  eta :: f a -> g a

instance Monoid mo => Natural f (Const mo) where
  eta = const (Const mempty)

-- | Product functor f and g
data Product f g a = Product (f a) (g a)
                     deriving Show
infixr 7 :*:
type (:*:) = Product

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap f (Product fa ga) = Product (fmap f fa) (fmap f ga)

instance (Applicative f, Applicative g) => Applicative (Product f g) where
  pure a = Product (pure a) (pure a)
  Product ff gf <*> Product fa ga = Product (ff <*> fa) (gf <*> ga)

-- | Coproduct functor f and g
data Coproduct f g a = InL (f a) | InR (g a)
                     deriving Show
infixr 6 :+:
type (:+:) = Coproduct

instance (Functor f, Functor g) => Functor (Coproduct f g) where
  fmap f (InL fa) = InL (fmap f fa)
  fmap f (InR ga) = InR (fmap f ga)

instance (Applicative f, Applicative g, Natural g f)
      => Applicative (Coproduct f g) where
  pure a = InR $ pure a
  InL ff <*> InL fa = InL (ff <*> fa)
  InR gf <*> InR ga = InR (gf <*> ga)
  InL ff <*> InR ga = InL (ff <*> eta ga)
  InR gf <*> InL fa = InL (eta gf <*> fa)

-- | Compose functor f and g
data Compose f g a = Compose (f (g a))
                     deriving Show
infixr 9 :.:
type (:.:) = Compose

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose (fmap (fmap f) fga)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure a = Compose (pure (pure a))
  Compose fgf <*> Compose fga = Compose $ (<*>) <$> fgf <*> fga
