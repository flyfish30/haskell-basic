{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE TypeOperators, TypeApplications, RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module FreeX () where

import Data.Monoid
import Data.Functor.Const
import Data.Default

safeHead :: Default a => [a] -> a
safeHead [] = def
safeHead (x:xs) = x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

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

-- ^ Impelement join for Monad
{-# INLINE join #-}
join :: Monad m => m (m a) -> m a
join = (>>= id)

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
monad (Pure a) = pure a
monad (Free mfr) = mfr >>= monad
-- monad (Free mfr) = join (fmap monad mfr)

-- F-Alg and Free Monad
type Alg f a = f a -> a

newtype Fix f = In { out :: f (Fix f) }
instance Show (f (Fix f)) => Show (Fix f) where
  show (In fix) = "(In (" ++ show fix ++ "))"

cata :: Functor f => Alg f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . out

-- Free monoid is List a
type FreeMon a = List a

data List a = Nil | Cons a (List a)
              deriving Show

data ListF a x = NilF | ConsF a x
                 deriving Show

instance Functor (ListF a) where
  fmap f NilF = NilF
  fmap f (ConsF a x) = ConsF a (f x)

instance Semigroup (Fix (ListF a)) where
  (In NilF) <> fixl2         = fixl2
  fixl1     <> (In NilF)     = fixl1
  (In (ConsF a l1)) <> fixl2 = In (ConsF a (mappend l1 fixl2))

instance Monoid (Fix (ListF a)) where
  mempty = In NilF
  mappend = (<>)

instance Semigroup (List a) where
  Nil <> ls2 = ls2
  ls1 <> Nil = ls1
  (Cons a ls1) <> ls2 = Cons a (ls1 <> ls2)

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

lengthAlg :: Alg (ListF a) Int
lengthAlg NilF = 0
lengthAlg (ConsF a x) = 1 + x

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

data MonF a = MEmptyF | MAppendF a a

algMon :: Monoid a => Alg MonF a
algMon MEmptyF = mempty
algMon (MAppendF a b) = mappend a b

class Monoid' m where
  monoid :: FreeMon m -> m

mempty' :: Monoid' m => m
mempty' = monoid Nil

mappend' :: Monoid' m => m -> m -> m
mappend' a b = monoid . Cons a $ Cons b Nil
-- mappend' = curry $ monoid . uncurry (flip Cons . flip Cons Nil)

instance Monoid' Int where
  monoid Nil = 0
  monoid (Cons a frm) = a + monoid frm

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

-- FreeMF f g a is same as FListF f g a
data FreeMF f g a = PureMF a | FreeMF (f (g a))
instance Functor f => HFunctor (FreeMF f) where
  hfmap nat (PureMF a)   = PureMF a
  hfmap nat (FreeMF fga) = FreeMF $ fmap nat fga
  ffmap f   (PureMF a)   = PureMF (f a)
  ffmap f   (FreeMF fga) = FreeMF $ fmap (fmap f) fga

-- Free f =  FList f = HFix (FListF f) = HFix (FreeMF f)
type FreeMonad f = HFix (FreeMF f)

algMonad :: Monad m => HAlg (FreeMF m) m
algMonad (PureMF a)   = pure a
algMonad (FreeMF fga) = join fga

class Functor m => Monad' m where
  monad' :: Free m :~> m

instance Monad' Maybe where
  monad' (Pure a) = Just a
  monad' (Free Nothing) = Nothing
  monad' (Free (Just frm)) = monad' frm

pure' :: Monad' m => a -> m a
pure' = monad' . Pure

join' :: Monad' m => m (m a) -> m a
join' = monad' . Free . fmap (Free . fmap Pure)

bind' :: Monad' m => m a -> (a -> m b) -> m b
bind' m k = join' (fmap k m)

liftMF :: Functor f => f a -> FreeMonad f a
liftMF = InH . FreeMF . fmap (InH . PureMF)

instance Functor f => Functor (HFix (FreeMF f)) where
  fmap f (InH frmf) = InH (ffmap f frmf)

instance Functor f => Applicative (HFix (FreeMF f)) where
  pure = InH . PureMF
  (InH (PureMF f))   <*> fixmf = fmap f fixmf
  (InH (FreeMF frf)) <*> fixmf = InH . FreeMF $ fmap (<*> fixmf) frf

instance Functor f => Monad (HFix (FreeMF f)) where
  (InH (PureMF a))   >>= k = k a
  (InH (FreeMF fga)) >>= k = InH . FreeMF $ fmap (>>= k) fga


-- Free Monad example
--
-- data Const a b = Const { getConst :: a }

data Writer w a = Writer { runWriter :: (a, w) }
                deriving Functor

instance Monoid w => Applicative (Writer w) where
  pure a = Writer (a, mempty)
  (Writer (f, w)) <*> (Writer (a, w1)) = Writer (f a, w `mappend` w1)

instance Monoid w => Monad (Writer w) where
  (Writer (a, w)) >>= k = Writer $ let (a1, w1) = runWriter (k a)
                                   in (a1, w `mappend` w1)

data State s a = State { runState :: s -> (a, s) }

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \s -> ((), s)

{-
instance (Show s, Show a) => Show (State s a) where
  show (State (\s -> (a, s))) = show s ++ " -> (" ++ show a ++ ", " ++ show s ++ ")"
-}

instance Functor (State s) where
  fmap f state = State $ \s -> let (a, s1) = runState state s
                               in  (f a, s1)

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  statef <*> state = State $ \s -> let (f, s1) = runState statef s
                                       (a, s2) = runState state s1
                                   in  (f a, s2)

instance Monad (State s) where
  state >>= k = State $ \s -> let (a1, s1) = runState state s
                              in  runState (k a1) s1

instance Monad' (State s) where
  monad' (Pure a) = State $ \s -> (a, s)
  monad' (Free state) = State $ \s -> let (a, s1) = runState state s
                                      in  runState (monad' a) s1

data StackF k = Push Int k
              | Pop k
              | Top (Int -> k)
              | Add k
              | Mul k
              deriving Functor

type FreeStack = Free StackF

push x = liftFree (Push x ())
pop    = liftFree (Pop ())
top    = liftFree (Top id)
add    = liftFree (Add ())
mul    = liftFree (Mul ())

calc = do 
  push 3
  push 4
  add
  push 5
  mul
  x <- top
  pop
  pure x

type MemState = State [Int]

-- | This function is used for demostrate f = free f . ins
interp :: (Functor f, Monad m) => (f :~> m) -> (Free f :~> m)
interp phi = monad . freeMap phi

phiRun :: StackF k -> MemState k
phiRun (Push a k) = State $ \s -> (k, a:s)
phiRun (Pop k)    = State $ \s -> (k, safeTail s)
phiRun (Top ik)   = State $ \s -> (ik (safeHead s), s)
phiRun (Add k)    = State $ \s@(x:y:ts) -> (k, (x + y) : ts)
phiRun (Mul k)    = State $ \s@(x:y:ts) -> (k, (x * y) : ts)
{-
phiRun (Push a k) = (State $ \s -> ((), a:s)) >> return k
phiRun (Pop k)    = (State $ \s -> ((), safeTail s)) >> return k
phiRun (Top ik)   = (State $ \s -> ((safeHead s), s)) >>= return . ik
phiRun (Add k)    = (State $ \s@(x:y:ts) -> ((), (x + y) : ts)) >> return k
phiRun (Mul k)    = (State $ \s@(x:y:ts) -> ((), (x * y) : ts)) >> return k
-}

phiShow :: StackF k -> Writer String k
phiShow (Push a k) = Writer (k, "Push " ++ show a ++ ", ")
phiShow (Pop k)    = Writer (k, "Pop, ")
phiShow (Top ik)   = Writer (ik 42, "Top, ")
phiShow (Add k)    = Writer (k, "Add, ")
phiShow (Mul k)    = Writer (k, "Mul, ")
{-
phiShow (Push a k) = Writer ((), "Push " ++ show a ++ ", ") >> return k
phiShow (Pop k)    = Writer ((), "Pop, ") >> return k
phiShow (Top ik)   = Writer (42, "Top, ") >>= return . ik
phiShow (Add k)    = Writer ((), "Add, ") >> return k
phiShow (Mul k)    = Writer ((), "Mul, ") >> return k
-}

interpState :: Free StackF Int -> MemState Int
interpState (Pure a) = return a
interpState (Free (Push a k)) = (State $ \s -> ((), a:s)) >> interpState k
interpState (Free (Pop k))  = (State $ \s -> ((), safeTail s)) >> interpState k
interpState (Free (Top ik)) = (State $ \s -> (ik (safeHead s), s)) >>= interpState
interpState (Free (Add k))  = (State $ \s@(x:y:ts) -> ((), (x + y) : ts)) >> interpState k
interpState (Free (Mul k))  = (State $ \s@(x:y:ts) -> ((), (x * y) : ts)) >> interpState k

-- Const String is not a monad, but can get generate all calculation string
interpShow :: Show a => Free StackF a -> Const String ()
interpShow (Pure a) = Const "Done!"
interpShow (Free (Push a k)) = Const $ "Push " ++ show a ++ ", " ++ getConst (interpShow k)
interpShow (Free (Pop k))  = Const $ "Pop, " ++ getConst (interpShow k)
interpShow (Free (Top ik)) = Const $ "Top, " ++ getConst (interpShow (ik 42))
interpShow (Free (Add k))  = Const $ "Add, " ++ getConst (interpShow k)
interpShow (Free (Mul k))  = Const $ "Mul, " ++ getConst (interpShow k)

pushF x = liftMF (Push x ())
popF    = liftMF (Pop ())
topF    = liftMF (Top id)
addF    = liftMF (Add ())
mulF    = liftMF (Mul ())

calcF = do 
  pushF 3
  pushF 4
  addF
  pushF 5
  mulF
  x <- topF
  popF
  return x

runAlg :: HAlg (FreeMF StackF) MemState
runAlg (PureMF a) = return a
runAlg (FreeMF (Push a k)) = (State $ \s -> ((), a:s)) >> k
runAlg (FreeMF (Pop k))  = (State $ \s -> ((), safeTail s)) >> k
runAlg (FreeMF (Top ik)) = get >>= ik . safeHead
runAlg (FreeMF (Add k))  = (State $ \s@(x:y:ts) -> ((), (x+y):ts)) >> k
runAlg (FreeMF (Mul k))  = (State $ \s@(x:y:ts) -> ((), (x*y):ts)) >> k

-- Const String is not a monad, but can get generate all calculation string
runShow :: HAlg (FreeMF StackF) (Const String)
runShow (PureMF a) = Const "Done!"
runShow (FreeMF (Push a k)) = Const $ "Push " ++ show a ++ ", " ++ getConst k
runShow (FreeMF (Pop k))  = Const $ "Pop, " ++ getConst k
runShow (FreeMF (Top ik)) = Const $ "Top, " ++ getConst (ik 42)
runShow (FreeMF (Add k))  = Const $ "Add, " ++ getConst k
runShow (FreeMF (Mul k))  = Const $ "Mul, " ++ getConst k

