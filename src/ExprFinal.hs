{-# LANGUAGE TypeInType, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ExprFinal where

import Prelude       hiding (and, or)
import Data.Char
import Data.Function
import Data.List
import Data.Proxy
import GHC.TypeLits

import Control.Applicative

data Expr env t where
  LitB :: Bool -> Expr env Bool
  LitI :: Int -> Expr env Int
  Var  :: DVar env t -> Expr env t
  Neg  :: Num a => Expr env a -> Expr env a
  Lam  :: Expr (a,env) b -> Expr env (a -> b)
  App  :: Expr env (a -> b) -> Expr env a -> Expr env b
  Add  :: Num a => Expr env a -> Expr env a -> Expr env a
  Sub  :: Num a => Expr env a -> Expr env a -> Expr env a
  Mul  :: Num a => Expr env a -> Expr env a -> Expr env a

data DVar env t where
  VZ :: DVar (t, env) t
  VS :: DVar env t -> DVar (a, env) t

instance Num (Expr env Int) where
  fromInteger = LitI . fromInteger
  (+) = Add
  (-) = Sub
  (*) = Mul
  negate = Neg
  abs = notImplemented "Expr.abs"
  signum = notImplemented "Expr.signum"

notImplemented :: String -> a
notImplemented err = error (err ++ "is not implemented")

lookupEnv :: DVar env t -> env -> t
lookupEnv VZ     (t, _)    = t
lookupEnv (VS v) (_, env') = lookupEnv v env'

eval :: env -> Expr env t -> t
eval env (LitB b) = b
eval env (LitI i) = i
eval env (Var v)  = lookupEnv v env
eval env (Neg e)  = negate $ eval env e
eval env (Lam e)  = \x -> eval (x, env) e
eval env (App ef ev) = eval env ef $ eval env ev
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2

-- ^ Implement final coalgebra of Expressing
class Language r where
  here   :: r (a, h) a
  before :: r h a -> r (any, h) a
  lambda :: r (a, h) b -> r h (a -> b)
  apply  :: r h (a -> b) -> r h a -> r h b

  loop   :: r h (a -> a) -> r h a

  int    :: Int -> r h Int
  add    :: r h Int -> r h Int -> r h Int
  mult   :: r h Int -> r h Int -> r h Int
  down   :: r h Int -> r h Int       -- \x -> x - 1
  up     :: r h Int -> r h Int       -- \x -> x + 1
  gte    :: r h Int -> r h Int -> r h Bool

  bool   :: Bool -> r h Bool
  or     :: r h Bool -> r h Bool -> r h Bool
  and    :: r h Bool -> r h Bool -> r h Bool
  neg    :: r h Bool -> r h Bool

  ifte   :: r h Bool -> r h a -> r h a -> r h a

type Term a = forall r h. Language r => r h a

interpret :: Term a -> a
interpret t = run t ()

newtype Prg h a = Prg { run :: h -> a } deriving Functor

instance Applicative (Prg h) where
  pure = Prg . const
  Prg f <*> Prg e = Prg $ \h -> f h $ e h

instance Language Prg where
  here     = Prg $ \(a, _) -> a
  before v = Prg $ \(_, h) -> run v h
  lambda e = Prg $ \h a -> run e (a, h)
  apply    = (<*>)

  loop = liftA fix

  int  = pure
  add  = liftA2 (+)
  mult = liftA2 (*)
  down = liftA (subtract 1)
  up   = liftA (+ 1)
  gte  = liftA2 (>=)

  bool = pure
  or   = liftA2 (||)
  and  = liftA2 (&&)
  neg  = liftA not

  ifte = liftA3 iff

iff :: Bool -> a -> a -> a
iff True  e1 e2 = e1
iff False e1 e2 = e2

fact :: Term (Int -> Int)
fact = let eq0 :: Term (Int -> Bool)
           eq0 = apply ieq (int 0)
           ieq :: Term (Int -> Int -> Bool)
           ieq = 
             lambda $
               lambda $
                 ExprFinal.and (gte here (before here))
                               (gte (before here) here)
           fact_ :: Term ((Int -> Int) -> (Int -> Int))
           fact_ = 
             lambda $
               lambda $ 
                 ifte (apply eq0 here) 
                      (int 1)
                      (mult here 
                            (apply (before here) (down here)))
       in loop fact_
