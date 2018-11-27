{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

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
instance Show (f (Fix f)) => Show (Fix f) where
  show (In fix) = "(In (" ++ show fix ++ "))"

cata :: Functor f => Alg f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . out

data List a = Nil | Cons a (List a)
              deriving Show

data ListF a x = NilF | ConsF a x
                 deriving Show
instance Functor (ListF a) where
  fmap f NilF = NilF
  fmap f (ConsF a x) = ConsF a (f x)

sumAlgListF :: Alg (ListF Int) Int
sumAlgListF NilF = 0
sumAlgListF (ConsF a l) = a + l

prodAlgListF :: Alg (ListF Int) Int
prodAlgListF NilF = 1
prodAlgListF (ConsF a l) = a * l

listAlg :: Alg (ListF a) (List a)
listAlg NilF = Nil
listAlg (ConsF a l) = Cons a l

ghcListAlg :: Alg (ListF a) [a]
ghcListAlg NilF = []
ghcListAlg (ConsF a l) = a : l

data MonF a = MEmpty | MAppend a a

algMon :: Monoid a => Alg MonF a
algMon MEmpty = mempty
algMon (MAppend a b) = mappend a b

-- Higher order F-Alg ------
infixr 0 :~>
type f :~> g = forall a. f a -> g a
type Natural f g = f :~> g

class HFunctor hf where
  hfmap :: (g :~> h) -> hf g :~> hf h
  ffmap :: Functor g => (a -> b) -> hf g a -> hf g b

type HAlg hf f = hf f :~> f

newtype HFix hf a = InH { outH :: hf (HFix hf) a }
instance Show (hf (HFix hf) a) => Show (HFix hf a) where
  show (InH hfix) = "(InH (" ++ show hfix ++ "))"

hcata :: (HFunctor hf, Functor f) => HAlg hf f -> HFix hf :~> f
hcata halg = halg . hfmap (hcata halg) . outH

data FList f a = FNil a | FCons (f (FList f a))
instance (Show (f (FList f a)), Show a) => Show (FList f a) where
  show (FNil a) = "FNil " ++ show a
  show (FCons fx) = "Fcons " ++ show fx

data FListF f g a = FNilF a | FConsF (f (g a))
instance (Show (f (g a)), Show a) => Show (FListF f g a) where
  show (FNilF a) = "FNilF " ++ show a
  show (FConsF fga) = "FConsF " ++ show fga

instance Functor f => HFunctor (FListF f) where
  hfmap nat (FNilF a) = FNilF a
  hfmap nat (FConsF fga) = FConsF (fmap nat fga)
  ffmap k (FNilF a) = FNilF (k a)
  ffmap k (FConsF fga) = FConsF (fmap (fmap k) fga)

algFreeM :: HAlg (FListF f) (Free f)
algFreeM (FNilF a) = Pure a
algFreeM (FConsF ffra) = Free ffra

