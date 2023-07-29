{-# LANGUAGE GADTs #-}

module StagedProgram where

import Data.Maybe
import qualified Data.Map as M
import Data.IORef
import GHC.IO (unsafeDupablePerformIO, unsafePerformIO)

data Expr = Var String
          | IntVal Int
          | Let String Expr Expr  -- Let var value body
          | Add Expr Expr
          | Mul Expr Expr
          deriving (Show)

type Env a = M.Map String a

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
eval env (Let var e body) = eval (M.insert var (eval env e) env) body
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2

-- | A function  for partial evaluate expression to PValue
pEval :: Env PValue -> LetlistRef -> Expr -> PValue
pEval env llref expr = go expr
  where
    recurse = \e -> pEval env llref e
    go (Var name) = fromJust $ M.lookup name env
    go (IntVal i) = Static i
    go (Let var e body) = case recurse e of
      (Static vi) -> pEval (M.insert var (Static vi) env) llref body
      (Dynamic vd) -> let lete = pushLetlist llref vd
                      in lete `seq` pEval (M.insert var (Dynamic lete) env)
                                          llref body

    go (Add e1 e2) = case (val1, val2) of
      (Static i1, Static i2) -> Static $ i1 + i2
      (Static 0, yp) -> yp
      (xp, Static 0) -> xp
      (xp, yp) -> Dynamic $ Add (asDyn xp) (asDyn yp)
      where val1 = pEval env llref e1
            val2 = pEval env llref e2
    go (Mul e1 e2) = case (val1, val2) of
      (Static i1, Static i2) -> Static $ i1 * i2
      (Static 0, yp) -> Static 0
      (xp, Static 0) -> Static 0
      (Static 1, yp) -> yp
      (xp, Static 1) -> xp
      (xp, yp) -> Dynamic $ Mul (asDyn xp) (asDyn yp)
      where val1 = pEval env llref e1
            val2 = pEval env llref e2

-- Define letlist type and some functions
type Letlist = [(String, Expr)]
type LetlistRef = IORef Letlist

pushLetlist :: LetlistRef -> Expr -> Expr
pushLetlist llref e = let var = freshName ()
                      in var `seq` unsafePerformIO
                      $ atomicModifyIORef llref
                      $ \ll -> let ll' = (var, e) : ll
                               in ll' `seq` (ll', Var var)
letLetlist :: Letlist -> Expr -> Expr
letLetlist ll expr = foldl (\e (n, v) -> Let n v e) expr ll
{-# INLINE letLetlist #-}

withLetList :: (LetlistRef -> Expr) -> Expr
withLetList f = expr `seq` letLetlist (unsafePerformIO $ readIORef llref) expr
  where llref = unsafePerformIO $ newIORef []
        expr = f llref

-- create a global variable varCounter for generate counter
varCounter :: IORef Int
varCounter = unsafePerformIO $ newIORef 0
{-# NOINLINE varCounter #-}

incCounter :: a -> IO Int
incCounter _ = atomicModifyIORef varCounter $
                                 \i -> let i' = i + 1 in i' `seq` (i', i)
{-# NOINLINE incCounter #-}

genCounter :: a -> Int
genCounter a = unsafePerformIO $ incCounter a
{-# NOINLINE genCounter #-}

freshVar () = genCounter ()
{-# INLINE freshVar #-}

freshName () = "x" ++ (show $ genCounter ())
