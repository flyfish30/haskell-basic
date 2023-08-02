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
          | Pair Expr Expr
          | Zro Expr
          | Fst Expr
          | LeftE Expr
          | RightE Expr
          | Match Expr (String, Expr) (String, Expr)
          | Fun String Expr  -- Fun var_name fun_body
          | App Expr Expr    -- App fun arg
          deriving (Show)

type Env a = M.Map String a

-- | Define a value for eval
data Value = VInt Int
           | VPair Value Value
           | VLeft Value
           | VRight Value
           | VFun (Value -> Value)

-- | Define a static value for pEval
data SValue = SInt Int
            | SPair PValue PValue
            | SLeft PValue
            | SRight PValue
            | SFun   (LetlistRef -> PValue -> PValue)

-- | Define a type for partial value
data PValue = Static SValue
            | Dynamic Expr

asDyn :: PValue -> Expr
asDyn (Static (SInt i))    = IntVal i
asDyn (Static (SPair l r)) = Pair (asDyn l) (asDyn r)
asDyn (Static (SLeft l))   = LeftE (asDyn l)
asDyn (Static (SRight r))  = RightE (asDyn r)
asDyn (Static (SFun lam))    = name `seq`
                             Fun name $
                                 withLetList (\llref ->
                                   asDyn $ lam llref (Dynamic $ Var name))
  where name = freshName ()
asDyn (Dynamic e) = e

-- | A function for evaluate expression to Value
eval :: Env Value -> Expr -> Value
eval env (Var name) = fromJust $ M.lookup name env
eval env (IntVal i) = VInt i
eval env (Add e1 e2) = case (val1, val2) of
  (VInt i1, VInt i2) -> VInt $ i1 + i2
  _                  -> error "Operands of Add isn't VInt value"
  where val1 = eval env e1
        val2 = eval env e2

eval env (Mul e1 e2) = case (val1, val2) of
  (VInt i1, VInt i2) -> VInt $ i1 * i2
  _                  -> error "Operands of Mul isn't VInt value"
  where val1 = eval env e1
        val2 = eval env e2

eval env (Let vn e body) = eval (M.insert vn (eval env e) env) body
eval env (IfZero c te fe) = case (eval env c) of
  VInt ci -> if ci == 0 then eval env te
                        else eval env fe
  _       -> error "Condition of IfZero isn't VInt value"

eval env (Pair e1 e2) = VPair val1 val2
  where val1 = eval env e1
        val2 = eval env e2

eval env (Zro e) = case eval env e of
  VPair l r -> l
  _         -> error "The value of Zro expression isn't VPair value"

eval env (Fst e) = case eval env e of
  VPair l r -> r
  _         -> error "The value of Fst expression isn't VPair value"

eval env (LeftE e)  = VLeft $ eval env e
eval env (RightE e) = VRight $ eval env e

eval env (Match e (lv, lb) (rv, rb)) = case eval env e of
  VLeft l  -> eval (M.insert lv l env) lb
  VRight r -> eval (M.insert rv r env) rb
  _        -> error "The value of Match expression isn't VLeft or VRight value"

eval env (Fun v b) = VFun $ \p -> eval (M.insert v p env) b

eval env (App f x) = case eval env f of
  VFun lam -> lam $ eval env x
  _       -> error "The function value of App isn't VFunc value"

-- | A function  for partial evaluate expression to PValue
pEval :: Env PValue -> LetlistRef -> Expr -> PValue
pEval env llref expr = go expr
  where
    recurse = \e -> pEval env llref e
    go (Var name) = fromJust $ M.lookup name env
    go (IntVal i) = Static $ SInt i

    go (Add e1 e2) = case (val1, val2) of
      (Static (SInt i1), Static (SInt i2)) -> Static $ SInt $ i1 + i2
      (Static (SInt 0), yp) -> yp
      (xp, Static (SInt 0)) -> xp
      (xp, yp) -> let tmpe = pushLetlist llref $ Add (asDyn xp) (asDyn yp)
                  in tmpe `seq` Dynamic tmpe
      where val1 = recurse e1
            val2 = recurse e2
    go (Mul e1 e2) = case (val1, val2) of
      (Static (SInt i1), Static (SInt i2)) -> Static $ SInt $ i1 * i2
      (Static (SInt 0), yp) -> Static $ SInt 0
      (xp, Static (SInt 0)) -> Static $ SInt 0
      (Static (SInt 1), yp) -> yp
      (xp, Static (SInt 1)) -> xp
      (xp, yp) -> let tmpe = pushLetlist llref $ Mul (asDyn xp) (asDyn yp)
                  in tmpe `seq` Dynamic tmpe
      where val1 = recurse e1
            val2 = recurse e2

    go (Let vn e body) = case recurse e of
      (Static vi) -> pEval (M.insert vn (Static vi) env) llref body
      (Dynamic vd) -> let lete = pushLetlist llref vd
                      in lete `seq` pEval (M.insert vn (Dynamic lete) env)
                                          llref body

    go (IfZero c te fe) = case recurse c of
      (Static (SInt vi)) -> if vi == 0 then recurse te else recurse fe
      (Dynamic vd) -> let tmpe = pushLetlist llref
                               $ IfZero vd (withLetList (\llref ->
                                              asDyn $ pEval env llref te))
                                           (withLetList (\llref ->
                                              asDyn $ pEval env llref fe))
                      in tmpe `seq` Dynamic tmpe
      _        -> error "The value of IfZero expression isn't SInt or dynamic"

    go (Pair l r) = Static $ SPair (recurse l) (recurse r)

    go (Zro e) = case recurse e of 
      (Static (SPair l r)) -> l
      (Dynamic p) -> let tmpe = pushLetlist llref $ Zro p
                     in tmpe `seq` Dynamic tmpe
      _           -> error "The value of Zro expression isn't SPair value"

    go (Fst e) = case recurse e of 
      (Static (SPair l r)) -> r
      (Dynamic p) -> let tmpe = pushLetlist llref $ Fst p
                     in tmpe `seq` Dynamic tmpe
      _           -> error "The value of Fst expression isn't SPair value"

    go (LeftE e) = Static $ SLeft (recurse e)
    go (RightE e) = Static $ SRight (recurse e)

    go (Match e (lv, lb) (rv, rb)) = case recurse e of
      (Static (SLeft l)) -> pEval (M.insert lv l env) llref lb
      (Static (SRight r)) -> pEval (M.insert rv r env) llref rb
      (Dynamic s) -> let tmpe = pushLetlist llref
                              $ Match s (lv, withLetList (\llref ->
                                  asDyn $ pEval (M.insert lv (Dynamic $ Var lv) env)
                                                llref lb))
                                        (rv, withLetList (\llref ->
                                  asDyn $ pEval (M.insert rv (Dynamic $ Var rv) env)
                                                llref rb))
                     in tmpe `seq` Dynamic tmpe
      _           -> error "The value of Match expression isn't SLeft or SRight value"

    go (Fun v b) = Static $ SFun (\llref p -> pEval (M.insert v p env) llref b)

    go (App f x) = case recurse f of
      (Static (SFun lam)) -> lam llref $ recurse x
      (Dynamic pf) -> let tmpe = pushLetlist llref
                               $ App pf (asDyn $ recurse x)
                       in tmpe `seq` Dynamic tmpe
      _           -> error "The function value of App expression isn't SFun value"

-- Define letlist type and some functions
type Letlist = [(String, Expr)]
type LetlistRef = IORef Letlist

pushLetlist :: LetlistRef -> Expr -> Expr
pushLetlist llref e = let vn = freshName ()
                      in vn `seq` unsafePerformIO
                      $ atomicModifyIORef llref
                      $ \ll -> let ll' = (vn, e) : ll
                               in ll' `seq` (ll', Var vn)

pushPValue :: LetlistRef -> PValue -> PValue
pushPValue llref pv = go pv
  where
    go (Static (SInt i))    = Static $ SInt i
    go (Static (SPair l r)) = Static $ SPair (pushPValue llref l)
                                             (pushPValue llref r)
    go (Static (SLeft l))   = Static $ SLeft (pushPValue llref l)
    go (Static (SRight r))  = Static $ SRight (pushPValue llref r)
    -- The function pushLetlist should modify llref, so it use seq to ensure
    -- pushLetlist must be evaluated before consturct Dynamic expression.
    go (Dynamic d)          = varExpr `seq` Dynamic varExpr
      where varExpr = pushLetlist llref d

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
