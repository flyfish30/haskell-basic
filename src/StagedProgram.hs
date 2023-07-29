{-# LANGUAGE GADTs #-}

module StagedProgram where

import Data.Maybe
import Data.Map as M

data Expr = Var String
          | IntVal Int
          | Let String Expr Expr  -- Let var value body
          | Add Expr Expr
          | Mul Expr Expr
          deriving (Show)

type Env a = Map String a

-- | Define a type for partial value
data PValue = Static Int
            | Dynamic Expr
            deriving (Show)

asDyn :: PValue -> Expr
asDyn (Static i)  = IntVal i
asDyn (Dynamic e) = e

-- | A function for evaluate expression to Int
eval :: Env Int -> Expr -> Int
eval env (Var name) = fromJust $ M.lookup name env
eval env (IntVal i) = i
eval env (Let var val body) = eval (insert var (eval env val) env) body
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2

-- | A function  for partial evaluate expression to PValue
pEval :: Env PValue -> Expr -> PValue
pEval env (Var name) = fromJust $ M.lookup name env
pEval env (IntVal i) = Static i
pEval env (Let var val body) = pEval (insert var (pEval env val) env) body

pEval env (Add e1 e2) = case (val1, val2) of
  (Static i1, Static i2) -> Static $ i1 + i2
  (Static 0, yp) -> yp
  (xp, Static 0) -> xp
  (xp, yp) -> Dynamic $ Add (asDyn xp) (asDyn yp)
  where val1 = pEval env e1
        val2 = pEval env e2
pEval env (Mul e1 e2) = case (val1, val2) of
  (Static i1, Static i2) -> Static $ i1 * i2
  (Static 0, yp) -> Static 0
  (xp, Static 0) -> Static 0
  (Static 1, yp) -> yp
  (xp, Static 1) -> xp
  (xp, yp) -> Dynamic $ Mul (asDyn xp) (asDyn yp)
  where val1 = pEval env e1
        val2 = pEval env e2
