{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Monoid

infixr 0 :~>
type f :~> g = forall a. f a -> g a
type Natural f g = f :~> g

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
  fmap f (PureA a)    = PureA (f a)
  fmap f (h :$: ffra) = fmap (f .) h :$: ffra

instance Functor f => Applicative (FreeA f) where
  pure = PureA
  (PureA g)    <*> x = fmap g x
  (h :$: ffrg) <*> x = fmap uncurry h :$: ((,) <$> ffrg <*> x)

-- Free Monad
data Free f a = Pure a
              | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure a)    = Pure (f a)
  fmap f (Free ffra) = Free (fmap (fmap f) ffra)

instance Functor f => Applicative (Free f) where
  pure = Pure
  (Pure g)    <*> x = fmap g x
  (Free ffrg) <*> x = Free (fmap (<*> x) ffrg)

instance Functor f => Monad (Free f) where
  return = Pure
  (Pure a)    >>= k = k a
  (Free ffra) >>= k = Free (fmap (>>= k) ffra)

concatFree :: Functor f => Free f (Free f a) -> Free f a
concatFree (Pure fr)   = fr
concatFree (Free frfr) = Free (fmap concatFree frfr)

liftFree :: Functor f => forall a. f a -> Free f a
liftFree = Free . fmap Pure

foldFree :: Functor f => forall a. (f a -> a) -> Free f a -> a
foldFree f (Pure a)    = a
foldFree f (Free ffra) = f $ fmap (foldFree f) ffra

-- hoistFree
freeMap :: (Functor f, Functor g) => (f :~> g) -> Free f a -> Free g a
freeMap phi (Pure a)    = Pure a
freeMap phi (Free ffra) = Free (phi (fmap (freeMap phi) ffra))

monad :: Monad m => Free m :~> m
monad (Pure a) = return a
monad (Free mfr) = do
  ma <- mfr
  monad ma

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
class HFunctor hf where
  hfmap :: (g :~> h) -> hf g :~> hf h
  ffmap :: Functor g => (a -> b) -> hf g a -> hf g b

type HAlg hf f = hf f :~> f

newtype HFix hf a = InH { outH :: hf (HFix hf) a }
instance Show (hf (HFix hf) a) => Show (HFix hf a) where
  show (InH hfix) = "(InH (" ++ show hfix ++ "))"

hcata :: (HFunctor hf, Functor f) => HAlg hf f -> HFix hf :~> f
hcata halg = halg . hfmap (hcata halg) . outH

-- FList f a is same as Free f a
data FList f a = FNil a | FCons (f (FList f a))
instance (Show (f (FList f a)), Show a) => Show (FList f a) where
  show (FNil a) = "FNil " ++ show a
  show (FCons fx) = "Fcons " ++ show fx

data FListF f g a = FNilF a | FConsF (f (g a))
instance (Show (f (g a)), Show a) => Show (FListF f g a) where
  show (FNilF a) = "FNilF " ++ show a
  show (FConsF fga) = "FConsF " ++ show fga

instance Functor f => HFunctor (FListF f) where
  hfmap nat (FNilF a)    = FNilF a
  hfmap nat (FConsF fga) = FConsF (fmap nat fga)
  ffmap k (FNilF a)    = FNilF (k a)
  ffmap k (FConsF fga) = FConsF (fmap (fmap k) fga)

algFreeM :: HAlg (FListF f) (Free f)
algFreeM (FNilF a) = Pure a
algFreeM (FConsF ffra) = Free ffra

-- FreeMF f g a is same as FListF f g a
data FreeMF f g a = PureMF a | FreeMF (f (g a))
instance Functor f => HFunctor (FreeMF f) where
  hfmap nat (PureMF a)   = PureMF a
  hfmap nat (FreeMF fga) = FreeMF $ fmap nat fga
  ffmap f   (PureMF a)   = PureMF (f a)
  ffmap f   (FreeMF fga) = FreeMF $ fmap (fmap f) fga

-- FList = HFix (FListF f) = HFix (FreeMF f)
instance Functor f => Functor (HFix (FreeMF f)) where
  fmap f (InH frmf) = InH (ffmap f frmf)

instance Functor f => Applicative (HFix (FreeMF f)) where
  pure = InH . PureMF
  (InH (PureMF f))   <*> hfix = fmap f hfix
  (InH (FreeMF frf)) <*> hfix = InH . FreeMF $ fmap (<*> hfix) frf

instance Functor f => Monad (HFix (FreeMF f)) where
  return = InH . PureMF
  (InH (PureMF a))   >>= k = k a
  (InH (FreeMF fga)) >>= k = InH . FreeMF $ fmap (>>= k) fga

