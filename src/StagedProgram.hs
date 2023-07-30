{-# LANGUAGE GADTs #-}

module StagedProgram where

import Debug.Trace
import Data.Maybe
import qualified Data.Map as M
import Data.IORef
import GHC.IO (unsafeDupablePerformIO, unsafePerformIO)

data Expr = Var String
          | IntVal Int
          | Add Expr Expr
          | Mul Expr Expr
          | Let String Expr Expr  -- Let var_name value body
          | IfZero Expr Expr Expr -- IfZero condition true false
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
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Let vn e body) = eval (M.insert vn (eval env e) env) body
eval env (IfZero c te fe) = if (eval env c) == 0 then eval env te
                                                 else eval env fe

-- | A function  for partial evaluate expression to PValue
pEval :: Env PValue -> LetlistRef -> Expr -> PValue
pEval env llref expr = go expr
  where
    recurse = \e -> pEval env llref e
    go (Var name) = fromJust $ M.lookup name env
    go (IntVal i) = Static i

    go (Add e1 e2) = case (val1, val2) of
      (Static i1, Static i2) -> Static $ i1 + i2
      (Static 0, yp) -> yp
      (xp, Static 0) -> xp
      (xp, yp) -> Dynamic $ Add (asDyn xp) (asDyn yp)
      where val1 = recurse e1
            val2 = recurse e2
    go (Mul e1 e2) = case (val1, val2) of
      (Static i1, Static i2) -> Static $ i1 * i2
      (Static 0, yp) -> Static 0
      (xp, Static 0) -> Static 0
      (Static 1, yp) -> yp
      (xp, Static 1) -> xp
      (xp, yp) -> Dynamic $ Mul (asDyn xp) (asDyn yp)
      where val1 = recurse e1
            val2 = recurse e2

    go (Let vn e body) = case recurse e of
      (Static vi) -> pEval (M.insert vn (Static vi) env) llref body
      (Dynamic vd) -> let lete = pushLetlist llref vd
                      in lete `seq` pEval (M.insert vn (Dynamic lete) env)
                                          llref body

    go (IfZero c te fe) = case recurse c of
      (Static vi) -> if vi == 0 then recurse te else recurse fe
      (Dynamic vd) -> Dynamic $ IfZero vd (withLetList (\llref -> asDyn $ pEval env llref te))
                                          (withLetList (\llref -> asDyn $ pEval env llref fe))

-- Define letlist type and some functions
type Letlist = [(String, Expr)]
type LetlistRef = IORef Letlist

pushLetlist :: LetlistRef -> Expr -> Expr
pushLetlist llref e = let vn = freshName ()
                      in vn `seq` unsafePerformIO
                      $ atomicModifyIORef llref
                      $ \ll -> let ll' = (vn, e) : ll
                               in ll' `seq` (ll', Var vn)
letLetlist :: Letlist -> Expr -> Expr
letLetlist ll expr = foldl (\e (n, v) -> Let n v e) expr ll
{-# INLINE letLetlist #-}

getLetlist :: LetlistRef -> Letlist
getLetlist llref = unsafePerformIO $ readIORef llref
{-# INLINE getLetlist #-}

withLetList :: (LetlistRef -> Expr) -> Expr
withLetList f = expr `seq` letLetlist (getLetlist llref) expr
  where llref = unsafePerformIO $ newIORef []
        expr = f llref

-- create a global variable varCounter for generate counter
varCounter :: IORef Int
varCounter = unsafePerformIO $ newIORef 0
{-# NOINLINE varCounter #-}

incCounter :: a -> IO Int
incCounter _ = atomicModifyIORef varCounter
             $ \i -> let i' = i + 1 in i' `seq` (i', i)
{-# NOINLINE incCounter #-}

genCounter :: a -> Int
genCounter a = unsafePerformIO $ incCounter a
{-# NOINLINE genCounter #-}

freshVar () = genCounter ()
{-# INLINE freshVar #-}

freshName () = "x" ++ (show $ genCounter ())
