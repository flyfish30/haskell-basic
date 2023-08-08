{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}

module StagedProgram where

import Debug.Trace
import Data.Maybe
import Data.Foldable  (foldl')
import qualified Data.Map as M
import Data.IORef
import GHC.IO (unsafeDupablePerformIO, unsafePerformIO)
import GHC.Generics (Generic)

import Control.DeepSeq

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
          deriving (Show, Generic, NFData)

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
            | SFun   (LetlistRef -> PValue -> PValue)   -- SFun lam

-- | Define a type for partial value
-- Force the dynamic value must be a variable name for avoid copy expression
data PValue = MkPValue { static :: Maybe SValue
                       , dyn :: String   -- variable name
                       }

mkVarVal name = MkPValue { static = Nothing
                         , dyn = name
                         }

{-
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
-}

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
  _        -> error "The match expression of Match isn't VLeft or VRight value"

eval env (Fun v b) = VFun $ \p -> eval (M.insert v p env) b

eval env (App f x) = case eval env f of
  VFun lam -> lam $ eval env x
  _       -> error "The function of App isn't VFunc value"

-- | A function for partial evaluate expression to PValue
pEval :: Env PValue -> LetlistRef -> Expr -> PValue
pEval env llref expr = go expr
  where
    recurse = \e -> pEval env llref e
    go (Var name) = fromJust $ M.lookup name env
    go (IntVal i) = pInt llref $ i

    go (Add e1 e2) = case (static val1, static val2) of
      (Just (SInt i1), Just (SInt i2)) -> pInt llref $ i1 + i2
      (Just (SInt 0), _) -> val2
      (_, Just (SInt 0)) -> val1
      _     -> mkDynamic llref $ Add (Var $ dyn val1) (Var $ dyn val2)
      where val1 = recurse e1
            val2 = recurse e2

    go (Mul e1 e2) = case (static val1, static val2) of
      (Just (SInt i1), Just (SInt i2)) -> pInt llref $ i1 * i2
      (Just (SInt 0), _) -> pInt llref $ 0
      (_, Just (SInt 0)) -> pInt llref $ 0
      (Just (SInt 1), _) -> val2
      (_, Just (SInt 1)) -> val1
      _     -> mkDynamic llref $ Mul (Var $ dyn val1) (Var $ dyn val2)
      where val1 = recurse e1
            val2 = recurse e2

    go (Let vn e body) = case static letv of
      Just vi -> pEval (M.insert vn letv env) llref body
      Nothing -> let letn = pushLetlist llref (Var $ dyn letv)
                 in letn `seq` pEval (M.insert vn (mkDynamic llref (Var letn)) env) llref body
      where letv = recurse e

    go (IfZero c te fe) = case static vc of
      Just (SInt vi) -> if vi == 0 then recurse te else recurse fe
      Just  _ -> error "The value of IfZero expression isn't SInt or dynamic"
      Nothing -> let tmpe = IfZero vd (withLetList (\llref ->
                                         Var $ dyn $ pEval env llref te))
                                      (withLetList (\llref ->
                                         Var $ dyn $ pEval env llref fe))
                 in mkDynamic llref tmpe
      where vc = recurse c
            vd = Var $ dyn vc

    go (Pair l r) = pPair llref (recurse l) (recurse r)

    go (Zro e) = case static val0 of
      Just (SPair l r) -> l
      Just  _ -> error "The value of Zro expression isn't SPair value"
      Nothing -> mkDynamic llref $ Zro p
      where val0 = recurse e
            p  = Var $ dyn val0

    go (Fst e) = case static val1 of
      Just (SPair l r) -> r
      Just  _ -> error "The value of Fst expression isn't SPair value"
      Nothing -> mkDynamic llref $ Fst p
      where val1 = recurse e
            p  = Var $ dyn val1

    go (LeftE e)  = pLeft  llref (recurse e)
    go (RightE e) = pRight llref (recurse e)

    go (Match e (lv, lb) (rv, rb)) = case static vm of
      Just (SLeft l)  -> pEval (M.insert lv l env) llref lb
      Just (SRight r) -> pEval (M.insert rv r env) llref rb
      Just  _ -> error "The match expression of Match isn't SLeft or SRight value"
      Nothing -> let tmpe = Match s (lv, withLetList (\llref ->
                              Var $ dyn $ pEval (M.insert lv (mkVarVal lv) env)
                                                llref lb))
                                    (rv, withLetList (\llref ->
                              Var $ dyn $ pEval (M.insert rv (mkVarVal rv) env)
                                                llref rb))
                 in mkDynamic llref tmpe
      where vm = recurse e
            s  = Var $ dyn vm

    go (Fun v b) = pFun llref $ (\llref p -> pEval (M.insert v p env) llref b)

    go (App f x) = case static vf of
      Just (SFun lam) -> lam llref $ recurse x
      Just  _ -> error "The function of App isn't SFun value"
      Nothing -> mkDynamic llref $ App pf (Var $ dyn $ recurse x)
      where vf = recurse f
            pf = Var $ dyn vf

-- Define letlist type and some functions
type Letlist = [(String, Expr)]
type LetlistRef = IORef Letlist

pushLetlist :: LetlistRef -> Expr -> String
-- Ignore it when the expression is only Var name
pushLetlist llref (Var name) = name
pushLetlist llref e = let vn = freshName ()
                      in vn `seq` unsafePerformIO
                      $ atomicModifyIORef llref
                      $ \ll -> let ll' = (vn, e) : ll
                               in ll' `seq` (ll', vn)

{-
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
-}

mkStatic :: LetlistRef -> SValue -> Expr -> PValue
mkStatic llref s d = vn `seq`
                     MkPValue { static = Just s
                              , dyn = vn
                              }
  where vn = pushLetlist llref d

mkDynamic :: LetlistRef -> Expr -> PValue
mkDynamic llref (Var name) =
                    MkPValue { static = Nothing
                             , dyn = name
                             }
mkDynamic llref d = vn `seq`
                    MkPValue { static = Nothing
                             , dyn = vn
                             }
  where vn = pushLetlist llref d

pInt llref i = i `seq` mkStatic llref (SInt i) (IntVal i)

pPair llref l r = l `seq` r `seq`
                  mkStatic llref (SPair l r)
                                 (Pair (Var $ dyn l) (Var $ dyn r))

pLeft llref l = l `seq`
                mkStatic llref (SLeft l)
                               (LeftE (Var $ dyn l))

pRight llref r = r `seq`
                 mkStatic llref (SRight r)
                                (RightE (Var $ dyn r))

pFun llref lam = name `seq` lame `seq`
                 mkStatic llref (SFun lam)
                                (Fun name lame)
  where name = freshName ()
        lame = withLetList(\llref ->
                 Var $ dyn $ lam llref (mkVarVal name))
        -- dynFun = mkDynamic llref (Fun name lame)

letLetlist :: Letlist -> Expr -> Expr
letLetlist ll expr = foldl' (\e (n, v) -> Let n v e) expr ll
{-# INLINE letLetlist #-}

getLetlist :: LetlistRef -> Letlist
getLetlist llref = unsafePerformIO $ readIORef llref
{-# INLINE getLetlist #-}

withLetList :: (LetlistRef -> Expr) -> Expr
withLetList f = expr `deepseq` letLetlist ll expr
  where llref = unsafePerformIO $ newIORef []
        ll = getLetlist llref
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
