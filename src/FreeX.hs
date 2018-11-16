{-# LANGUAGE ExistentialQuantification #-}

import Data.Monoid

-- Free Functor
data FreeF f a = forall x. FreeF (x -> a) (f x)

instance Functor (FreeF f) where
  fmap f (FreeF g x) = FreeF (f . g) x

-- Free Applicative
class Functor f => MultiFunctor f where
  fmap0 :: a -> f a
  fmap1 :: (a -> b) -> f a -> f b
  fmap1 = fmap
  fmap2 :: (a -> b -> c) -> f a -> f b -> fc

fmap3 :: MultiFunctor f
      => (a -> b -> c -> d)
      -> f a -> f b -> f c -> f d
fmap3 f x y z = fmap2 ($) (fmap2 f x y) z

-- (<*>) = fmap2 ($)
-- fmap2 h x y = fmap h x <*> y

infixr 4 :$:
data FreeA f a = PureA a
               | forall b. f (b -> a) :$: FreeA f b

instance Functor f => Functor (FreeA f) where
  fmap f (PureA a) = PureA (f a)
  fmap f (h :$: fra) = fmap (f .) h :$: fra

instance Functor f => Applicative (FreeA f) where
  pure = PureA
  (PureA f) <*> fra = fmap f fra
  (h :$: frf) <*> fra = fmap uncurry h :$: ((,) <$> frf <*> fra)

-- Free Monad
data Free f a = Pure a
              | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure a)  = Pure (f a)
  fmap f (Free ff) = Free (fmap (fmap f) ff)

instance Functor f => Applicative (Free f) where
  pure = Pure
  (Pure f)  <*> x = fmap f x
  (Free ff) <*> x = Free (fmap (<*> x) ff)

instance Functor f => Monad (Free f) where
  return = Pure
  (Pure a)  >>= f = f a
  (Free ff) >>= f = Free (fmap (>>= f) ff)

concatFree :: Functor f => Free f (Free f a) -> Free f a
concatFree (Pure fr) = fr
concatFree (Free ff) = Free (fmap concatFree ff)

liftFree :: Functor f => forall a. f a -> Free f a
liftFree = Free . fmap Pure

foldFree :: Functor f => forall a. (f a -> a) -> Free f a -> a
foldFree f (Pure a) = a
foldFree f (Free ff) = f $ fmap (foldFree f) ff

-- F-Alg and Free Monad
type Alg f a = f a -> a

newtype Fix f = In { out :: f (Fix f) }

cata :: Functor f => Alg f a -> Fix f -> a
cata f = f . fmap (cata f) . out

data List a = Nil | Cons a (List a)

data ListF e a = NilF | ConsF e a
instance Functor (ListF e) where
  fmap f NilF = NilF
  fmap f (ConsF e a) = ConsF e (f a)

sumAlgListF :: Alg (ListF Int) Int
sumAlgListF NilF = 0
sumAlgListF (ConsF x l) = x + l

prodAlgListF :: Alg (ListF Int) Int
prodAlgListF NilF = 1
prodAlgListF (ConsF x l) = x * l

data MonF a = MEmpty | MAppend a a

algMon :: Monoid a => MonF a -> a
algMon MEmpty = mempty
algMon (MAppend a b) = a <> b
