{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}

module FirstClassFamilies where

import Prelude   hiding (fst, snd)
import Data.Kind (Constraint, Type)
import GHC.TypeLits

---------------------------------
-- Defunctionalize function
---------------------------------

fst :: (a, b) -> a
fst (a, b) = a

snd :: (a, b) -> b
snd (a, b) = b

-- Defunctionalize fst and snd type
data FstDf a b = FstDf (a, b)

data SndDf a b = SndDf (a, b)

-- Define EvalDf type class for evaluate defunctinalization type
class EvalDf l t | l -> t where
  eval :: l -> t

instance EvalDf (FstDf a b) a where
  eval (FstDf (a, b)) = a

instance EvalDf (SndDf a b) b where
  eval (SndDf (a, b)) = b

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

-- Defunctionalize listToMaybe
data ListToMaybeDf a = ListToMaybeDf [a]

instance EvalDf (ListToMaybeDf a) (Maybe a) where
  eval (ListToMaybeDf [])    = Nothing
  eval (ListToMaybeDf (x:_)) = Just x

-- Defunctionalize high order function
data MapListDf dfb a = MapListDf (a -> dfb) [a]

instance EvalDf dfb dft => EvalDf (MapListDf dfb a) [dft] where
  eval (MapListDf f [])     = []
  eval (MapListDf f (x:xs)) = eval (f x) : eval (MapListDf f xs)

---------------------------------
-- Defunctionalize type-level function (type family)
---------------------------------

type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

data Fst :: (a, b) -> Exp a
type instance Eval (Fst '(a, b)) = a

data Snd :: (a, b) -> Exp b
type instance Eval (Snd '(a, b)) = b

data And :: Bool -> Bool -> Exp Bool
type instance Eval (And 'False _t)    = 'False
type instance Eval (And 'True 'False) = 'False
type instance Eval (And 'True 'True)  = 'True

data Or :: Bool -> Bool -> Exp Bool
type instance Eval (Or 'True _t)      = 'True
type instance Eval (Or 'False 'True)  = 'True
type instance Eval (Or 'False 'False) = 'False

data Not :: Bool -> Exp Bool
type instance Eval (Not 'True) = 'False
type instance Eval (Not 'False) = 'True

data Error :: Symbol -> Exp a
type instance Eval (Error msg) = TypeError ('Text msg)

data TError :: ErrorMessage -> Exp a
type instance Eval (TError msg) = TypeError msg


data If :: Bool -> Exp a -> Exp a -> Exp a
type instance Eval (If 'True  e1 e2) = Eval e1
type instance Eval (If 'False e1 e2) = Eval e2

data FromMaybe :: a -> Maybe a -> Exp a
type instance Eval (FromMaybe _t ('Just a)) = a
type instance Eval (FromMaybe a  'Nothing)  = a

data (++) :: [a] -> [a] -> Exp [a]
type instance Eval ('[] ++ ts) = ts
type instance Eval ((a ': as) ++ ts) = a ': Eval (as ++ ts)

data ListToMaybe :: [a] -> Exp (Maybe a)
type instance Eval (ListToMaybe '[])       = 'Nothing
type instance Eval (ListToMaybe (a ': as)) = 'Just a

-- Defunctionalize high order type-level function
data MapList :: (a -> Exp b) -> [a] -> Exp [b]
type instance Eval (MapList f '[]) = '[]
type instance Eval (MapList f (a ': as)) = Eval (f a) ': Eval (MapList f as)

data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b
type instance Eval (Foldr f b '[])       = b
type instance Eval (Foldr f b (a ': as)) = Eval (f a (Eval (Foldr f b as)))

data Find :: (a -> Exp Bool) -> [a] -> Exp (Maybe a)
type instance Eval (Find _p '[]) = 'Nothing
type instance Eval (Find p (a ': as)) = Eval (If (Eval (p a))
                                                 (Pure ('Just a))
                                                 (Find p as))

data Uncurry :: (a -> b -> c) -> (a, b) -> Exp c
type instance Eval (Uncurry f '(a, b)) = f (Eval (Fst '(a, b))) (Eval (Snd '(a, b)))

data ConstFn :: a -> b -> Exp a
type instance Eval (ConstFn a _b) = a

data (<$>) :: (a -> b) -> Exp a -> Exp b
type instance Eval (f <$> e) = f (Eval e)

data (<*>) :: Exp (a -> b) -> Exp a -> Exp b
type instance Eval (f <*> e) = Eval f (Eval e)

data Pure :: a -> Exp a
type instance Eval (Pure a) = a

data Pure1 :: (a -> b) -> a -> Exp b
type instance Eval (Pure1 f a) = f a

data Pure2 :: (a -> b -> c) -> a -> b -> Exp c
type instance Eval (Pure2 f a b) = f a b

-- type-level as like function application ($)
infixr 0  =<<
data (=<<) :: (a -> Exp b)
           -> Exp a
           -> Exp b
type instance Eval (k =<< e) = Eval (k (Eval e))
type LiftM = (=<<)

data LiftM2 :: (a -> b -> Exp c)
            -> Exp a
            -> Exp b
            -> Exp c
type instance Eval (LiftM2 f e1 e2) = Eval (f (Eval e1) (Eval e2))

-- type-level as like function composition (.)
infixr 0 <=<
data (<=<) :: (b -> Exp c)
           -> (a -> Exp b)
           -> (a -> Exp c)
type instance Eval ((g <=< f) a) = Eval (g (Eval (f a)))

-- type-level monad join function
data Join :: Exp (Exp a) -> Exp a
type instance Eval (Join e) = Eval (Eval e)

-- Snd2 as like Snd . Snd
type Snd2 = Snd <=< Snd

-- Some useful type-level functions
data TyEq :: a -> b -> Exp Bool
type instance Eval (TyEq a b) = TyEqImpl a b

type family TyEqImpl (a :: k) (b :: k) :: Bool where
  TyEqImpl a a = 'True
  TyEqImpl a b = 'False

data Collapse :: [Constraint] -> Exp Constraint
type instance Eval (Collapse '[]) = (() :: Constraint)
type instance Eval (Collapse (c ': cs)) = (c, Eval (Collapse cs))

type All (c :: k -> Constraint) (ts :: [k]) = Collapse =<< (MapList (Pure1 c) ts)

data Map :: (a -> Exp b) -> f a -> Exp (f b)
type instance Eval (Map f '[]) = '[]
type instance Eval (Map f (a ': as)) = Eval (f a) ': Eval (Map f as)

type instance Eval (Map f 'Nothing) = 'Nothing
type instance Eval (Map f ('Just a)) = 'Just (Eval (f a))

type instance Eval (Map f ('Left x)) = 'Left x
type instance Eval (Map f ('Right a)) = 'Right (Eval (f a))

type instance Eval (Map f '(x, a)) = '(x, Eval (f a))

-- type-level monoid
data Mappend :: a -> a -> Exp a
type instance Eval (Mappend '() '()) = '()
type instance Eval (Mappend (a :: Constraint) (b :: Constraint)) = (a, b)
type instance Eval (Mappend (a :: [k]) (b :: [k])) = Eval (a ++ b)

data Mempty :: a -> Exp a
type instance Eval (Mempty '()) = '()
type instance Eval (Mempty (c :: Constraint)) = (() :: Constraint)
type instance Eval (Mempty (l :: [k])) = ('[] :: [k])

-- * Case spliting

infix 0 -->

data Match j k = Match_ j k
               | Is_ (j -> Exp Bool) k
               | Any_ k
               | Else_ (j -> Exp k)

-- Equivalent of case syntax in term-level
-- Matching exact value
-- type BoolToNat = Case
--   [ 'False --> 0
--   , 'True  --> 1
--   ]
--
-- Final matches for any value
-- type NatToBool = Case
--   [ 0 --> 'False
--   , Any 'True
--   ]
--
-- Final matches pass value to subcomputation
-- type ZeroOneOrSucc = Case
--   [ 0 --> 0
--   , 1 --> 1
--   , Else (Pure1 ((+) 1))
--   ]

data Case :: [Match j k] -> j -> Exp k
type instance Eval (Case ms a) = Case_ ms a

type family Case_ (ms :: [Match j k]) (a :: j) :: k where
  Case_ ('Match_ a b ': _)  a = b
  Case_ ('Match_ _ _ ': ms) a = Case_ ms a
  Case_ ('Is_ p b    ': ms) a = Case_ [ 'True  --> a
                                     , 'False --> Case_ ms a
                                     ] (Eval (p a))
  Case_ ('Any_ b     ': ms) a = b
  Case_ ('Else_ f    ': ms) a = Eval (f a) 

type (-->) = 'Match_
type Is    = 'Is_
type Any   = 'Any_
type Else  = 'Else_

