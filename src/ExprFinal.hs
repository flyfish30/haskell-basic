{-# LANGUAGE TypeInType, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module ExprFinal where

import Prelude       hiding (and, or)
import Data.Char
import Data.List
import Data.Proxy
import GHC.TypeLits

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

newtype RunR h a = RunR { unR :: h -> a }

instance Language RunR where
  here      = RunR $ \(a, _) -> a
  before v  = RunR $ \(_, h) -> unR v h
  lambda e  = RunR $ \h a -> unR e (a, h)
  apply f e = RunR $ \h -> unR f h $ unR e h

  loop f = let fixed = apply f (loop f) in fixed

  int  i     = RunR $ const i
  add  e1 e2 = RunR $ \h -> unR e1 h + unR e2 h
  mult e1 e2 = RunR $ \h -> unR e1 h * unR e2 h
  down x     = RunR $ \h -> unR x h - 1
  up   x     = RunR $ \h -> unR x h + 1
  gte  x  y  = RunR $ \h -> unR x h >= unR y h

  bool b     = RunR $ const b
  or   b1 b2 = RunR $ \h -> unR b1 h || unR b2 h
  and  b1 b2 = RunR $ \h -> unR b1 h && unR b2 h
  neg  b     = RunR $ \h -> not $ unR b h

  ifte p e1 e2 = RunR $ \h -> if (unR p h) then (unR e1 h) else (unR e2 h)

run :: Term a -> env -> a
run = unR

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
