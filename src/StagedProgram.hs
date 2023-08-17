{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module StagedProgram where

import Debug.Trace
import Data.Maybe
import Data.Foldable  (foldl')
import qualified Data.Map as M
import Data.IORef
import GHC.IO (unsafeDupablePerformIO, unsafePerformIO)
import GHC.Generics (Generic)
import Control.Monad.State

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
          | Count            -- counter effect
          deriving (Show, Generic, NFData)

type Env a = M.Map String a

type World = Int
type WorldRef = IORef World

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
            | SFun   (LetlistRef -> PValue -> (PValue, Maybe Int))  -- SFun lam

-- | Define a type for partial value
-- Force the dynamic value must be a variable name for avoid copy expression
data PValue = MkPValue { static :: Maybe SValue
                       , dyn :: String   -- variable name
                       }

mkIntVal i = MkPValue { static = Just (SInt i)
                      , dyn = ""
                      }

mkVarVal name = MkPValue { static = Nothing
                         , dyn = name
                         }

-- | definition a effective impure value
type EffValue = State (Maybe Int) PValue


mergeCnt (Just a) (Just b) = if a == b then Just a else Nothing
mergeCnt _        _        = Nothing

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

dynS (a, s) = (Var $ dyn $ a, s)

-- | A function for evaluate expression to Value
eval :: Env Value -> WorldRef -> Expr -> Value
eval env wref (Var name) = fromJust $ M.lookup name env
eval env wref (IntVal i) = VInt i
eval env wref (Add e1 e2) = case (val1, val2) of
  (VInt i1, VInt i2) -> VInt $ i1 + i2
  _                  -> error "Operands of Add isn't VInt value"
  where val1 = eval env wref e1
        val2 = eval env wref e2

eval env wref (Mul e1 e2) = case (val1, val2) of
  (VInt i1, VInt i2) -> VInt $ i1 * i2
  _                  -> error "Operands of Mul isn't VInt value"
  where val1 = eval env wref e1
        val2 = eval env wref e2

eval env wref (Let vn e body) = eval (M.insert vn (eval env wref e) env) wref body
eval env wref (IfZero c te fe) = case (eval env wref c) of
  VInt ci -> if ci == 0 then eval env wref te
                        else eval env wref fe
  _       -> error "Condition of IfZero isn't VInt value"

eval env wref (Pair e1 e2) = VPair val1 val2
  where val1 = eval env wref e1
        val2 = eval env wref e2

eval env wref (Zro e) = case eval env wref e of
  VPair l r -> l
  _         -> error "The value of Zro expression isn't VPair value"

eval env wref (Fst e) = case eval env wref e of
  VPair l r -> r
  _         -> error "The value of Fst expression isn't VPair value"

eval env wref (LeftE e)  = VLeft $ eval env wref e
eval env wref (RightE e) = VRight $ eval env wref e

eval env wref (Match e (lv, lb) (rv, rb)) = case eval env wref e of
  VLeft l  -> eval (M.insert lv l env) wref lb
  VRight r -> eval (M.insert rv r env) wref rb
  _        -> error "The match expression of Match isn't VLeft or VRight value"

eval env wref (Fun v b) = VFun $ \p -> eval (M.insert v p env) wref b

eval env wref (App f x) = case eval env wref f of
  VFun lam -> lam $ eval env wref x
  _       -> error "The function of App isn't VFunc value"

eval env wref Count = unsafePerformIO
                    $ atomicModifyIORef wref
                                        (\w -> let w' = w + 1 in (w', VInt w))

-- | A function for partial evaluate expression to PValue
pEval :: Env PValue -> LetlistRef -> Expr -> EffValue
pEval env llref expr = go expr
  where
    recurse = \e -> pEval env llref e
    go (Var name) = return $ fromJust $ M.lookup name env
    go (IntVal i) = return $ pInt llref $ i

    go (Add e1 e2) = do
      val1 <- recurse e1
      val2 <- recurse e2
      return $
        case (static val1, static val2) of
          (Just (SInt i1), Just (SInt i2)) -> pInt llref $ i1 + i2
          (Just (SInt 0), _) -> val2
          (_, Just (SInt 0)) -> val1
          _     -> mkDynamic llref $ Add (Var $ dyn val1) (Var $ dyn val2)

    go (Mul e1 e2) = do
      val1 <- recurse e1
      val2 <- recurse e2
      return $
        case (static val1, static val2) of
          (Just (SInt i1), Just (SInt i2)) -> pInt llref $ i1 * i2
          (Just (SInt 0), _) -> pInt llref $ 0
          (_, Just (SInt 0)) -> pInt llref $ 0
          (Just (SInt 1), _) -> val2
          (_, Just (SInt 1)) -> val1
          _     -> mkDynamic llref $ Mul (Var $ dyn val1) (Var $ dyn val2)

    go (Let vn e body) = do
      letv <- recurse e
      case static letv of
        Just vi -> pEval (M.insert vn letv env) llref body
        Nothing -> let letn = pushLetlist llref (Var $ dyn letv)
                   in letn `seq`
                      pEval (M.insert vn (mkDynamic llref (Var letn)) env)
                            llref body

    go (IfZero c te fe) = do
      pc <- recurse c
      let vd = Var $ dyn pc
      case static pc of
        Just (SInt cv) -> if cv == 0 then recurse te else recurse fe
        Just  _ -> error "The value of IfZero expression isn't SInt or dynamic"
        Nothing -> do
          mc <- get
          let (le, lc) = withLetList' (\llref ->
                           dynS ( runState (pEval env llref te) mc))
          let (re, rc) = withLetList' (\llref ->
                           dynS $ runState (pEval env llref fe) mc)
          put $ mergeCnt lc rc
          return $ mkDynamic llref $ IfZero vd le re

    go (Pair l r) = do
      vl <- recurse l
      vr <- recurse r
      return $ pPair llref (vl) (vr)

    go (Zro e) = do
      val0 <- recurse e
      let p = Var $ dyn val0
      return $
        case static val0 of
          Just (SPair l r) -> l
          Just  _ -> error "The value of Zro expression isn't SPair value"
          Nothing -> mkDynamic llref $ Zro p

    go (Fst e) = do
      val1 <- recurse e
      let p = Var $ dyn val1
      return $
        case static val1 of
          Just (SPair l r) -> r
          Just  _ -> error "The value of Fst expression isn't SPair value"
          Nothing -> mkDynamic llref $ Fst p

    go (LeftE e)  = do
      vl <- recurse e
      return $ pLeft llref vl
    go (RightE e) = do
      vr <- recurse e
      return $ pRight llref vr

    go (Match e (lv, lb) (rv, rb)) = do
      vm <- recurse e
      let s = Var $ dyn vm
      case static vm of
        Just (SLeft l)  -> pEval (M.insert lv l env) llref lb
        Just (SRight r) -> pEval (M.insert rv r env) llref rb
        Just  _ -> error "The match expression of Match isn't SLeft or SRight value"
        Nothing -> do
          mc <- get
          let (le, lc) = withLetList' (\llref ->
                           dynS $ runState (pEval (M.insert lv (mkVarVal lv) env)
                                                  llref lb)
                                           mc)
          let (re, rc) = withLetList' (\llref ->
                           dynS $ runState (pEval (M.insert rv (mkVarVal rv) env)
                                                  llref rb)
                                           mc)
          put $ mergeCnt lc rc
          return $ mkDynamic llref $ Match s (lv, le) (rv, re)

    go (Fun v b) = do
      mc <- get
      return $ pFun llref (\llref p -> runState (pEval (M.insert v p env) llref b) mc)

    go (App f x) = do
      pf <- recurse f
      px <- recurse x
      case static pf of
        Just (SFun lam) -> let (v, s) = lam llref px
                           in put s >> return v
        Just  _ -> error "The function of App isn't SFun value"
        Nothing -> do
          put Nothing
          return $ mkDynamic llref $ App (Var $ dyn $ pf) (Var $ dyn $ px)

    go Count = do
      mc <- get
      case mc of
        Just i -> put (Just (i + 1)) >> (return $ pInt llref i)
        Nothing -> return $ mkDynamic llref Count

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
        (lame, _) =
               withLetList'(\llref ->
                 dynS $ lam llref (mkVarVal name))
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

withLetList' :: (LetlistRef -> (Expr, Maybe Int)) -> (Expr, Maybe Int)
withLetList' f = expr `deepseq` (letLetlist ll expr, w)
  where llref = unsafePerformIO $ newIORef []
        ll = getLetlist llref
        (expr, w) = f llref

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
