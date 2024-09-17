{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE TypeOperators, TypeApplications, RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module ApplicativeAlg () where

import Prelude hiding (Applicative, (<*>), (<$>), pure)

-- define Applicative Functor
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

(<$>) :: Applicative f => (a -> b) -> f a -> f b
f <$> fa = pure f <*> fa

-- Applicative functor laws
-- pure id <*> v = v                            -- Identity
-- pure f <*> pure x = pure (f x)               -- Homomorphism
-- u <*> pure y = pure ($ y) <*> u              -- Interchange
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition

-- define MultiFunctor
class Functor f => MultiFunctor f where
  fmap0 :: a -> f a
  fmap1 :: (a -> b) -> f a -> f b
  fmap1 = fmap
  fmap2 :: (a -> b -> c) -> f a -> f b -> f c

fmap3 :: MultiFunctor f
      => (a -> b -> c -> d)
      -> f a -> f b -> f c -> f d
fmap3 f a b c = fmap2 ($) (fmap2 f a b) c

fmap4 :: MultiFunctor f
      => (a -> b -> c -> d -> e)
      -> f a -> f b -> f c -> f d -> f e
fmap4 f a b c d = fmap2 ($) (fmap3 f a b c) d

-- fmapN f a b c d .. y z = fmap2 ($) (fmapN_1 f a b c .. y) z
--
-- MultiFunctor define Applicative
-- pure = fmap0
-- (<*>) = fmap2 ($)
-- (<$>) = fmap1
--
-- Applicative Functor define MultiFunctor
-- fmap0 = pure
-- fmap1 = (<$>)
-- fmap2 f a b = pure f <*> a <*> b

-- define Lax Monoidal Functor
class Functor f => Monoidal f where
  unit :: f ()
  mult :: f a -> f b -> f (a, b)

-- Lax Monoidal Functor define Applicative
-- pure a = fmap (const a) unit
-- a <*> b = fmap (uncurry id) $ mult a b
--
-- Applicative Functor define Lax Monoidal Functor
-- unit = pure ()
-- mult a b = fmap (,) a <*> b


instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  _ <*> Nothing = Nothing
  Just f <*> Just a = Just (f a)

instance MultiFunctor Maybe where
  fmap0 = Just
  fmap2 _ Nothing _ = Nothing
  fmap2 _ _ Nothing = Nothing
  fmap2 f (Just a) (Just b) = Just (f a b)

instance Monoidal Maybe where
  unit = Just ()
  mult Nothing _ = Nothing
  mult _ Nothing = Nothing
  mult (Just a) (Just b) = Just (a, b)


newtype Constant a b = Constant a

instance Functor (Constant a) where
  fmap f (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant x <*> Constant y = Constant (x <> y)

instance Monoid a => MultiFunctor (Constant a) where
  fmap0 _ = Constant mempty
  fmap2 f (Constant x) (Constant y) = Constant (x <> y)

instance Monoid a => Monoidal (Constant a) where
  unit = Constant mempty
  mult (Constant x) (Constant y) = Constant (x <> y)

-- Fixed Points, Limits and Colimits
data ZipList a = Nil | Cons a (ZipList a)

instance Functor ZipList where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Monoidal ZipList where
  unit = Cons () unit
  mult (Cons x xs) (Cons y ys) = Cons (x, y) $ mult xs ys
  mult _ _ = Nil
