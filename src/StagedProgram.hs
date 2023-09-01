{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module StagedProgram where

import Debug.Trace
import Data.Maybe
import Data.Foldable  (foldl')
import qualified Data.Map as M
import qualified Data.Map.Merge.Strict as M
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
          | Unit
          | MkRef Expr
          | GetRef Expr      -- GetRef ref
          | SetRef Expr Expr -- SetRef ref value
          deriving (Show, Generic, NFData)

type Env a = M.Map String a

type CountRef = IORef Int

-- | Define a value for eval
data Value = VInt Int
           | VPair Value Value
           | VLeft Value
           | VRight Value
           | VFun (Value -> Value)
           | VUnit
           | VRef (IORef Value)

instance Show Value where
  show (VInt i) = "VInt " ++ show i
  show (VPair p1 p2) = "VPair (" ++ show p1
                     ++ ", " ++ show p2 ++ ")"
  show (VLeft l)  = "VLeft " ++ show l
  show (VRight r) = "VLeft " ++ show r
  show (VFun f)   = "VFun " ++ show (f VUnit)
  show VUnit      = "VUnit"
  show (VRef r)   = "VRef " ++ show (unsafePerformIO $ readIORef r)

-- | Define a static value for pEval
data SValue = SInt Int
            | SPair PValue PValue
            | SLeft PValue
            | SRight PValue
            | SFun   (LetlistRef -> PValue -> EffValue)  -- SFun lam
            | SUnit
            | SRef Int

instance Show SValue where
  show (SInt i) = "SInt " ++ show i
  show (SPair p1 p2) = "SPair (" ++ show p1
                     ++ ", " ++ show p2 ++ ")"
  show (SLeft l)  = "SLeft " ++ show l
  show (SRight r) = "SLeft " ++ show r
  show (SFun f)   = "SFun " ++ show (runState (f (unsafePerformIO $ newIORef [])
                                                 (mkVarVal ""))
                                              (MkWorld Nothing M.empty))
  show SUnit      = "SUnit"
  show (SRef i)   = "SRef " ++ show i

-- | Define a type for partial value
-- Force the dynamic value must be a variable name for avoid copy expression
data PValue = MkPValue { static :: Maybe SValue
                       , dyn :: String   -- variable name
                       }
            deriving (Show)


mkIntVal i = MkPValue { static = Just (SInt i)
                      , dyn = ""
                      }

mkVarVal name = MkPValue { static = Nothing
                         , dyn = name
                         }

-- | Define a World type for effect feature
data World = MkWorld { counter :: Maybe Int
                     , store   :: M.Map Int PValue
                     }

instance Show World where
  show w@(MkWorld mc st) = show "{ counter=" ++ show mc
                         ++ show ", store=" ++ show (M.toList st)
                         ++ show " }"

emptyWorld = MkWorld { counter = Nothing
                     , store   = M.empty
                     }

mergeCnt (Just a) (Just b) = if a == b then Just a else Nothing
mergeCnt _        _        = Nothing

mergeStore a b = M.merge M.dropMissing M.dropMissing (M.zipWithMaybeMatched f)
                         a b
  where f _ av bv = if (dyn av) == (dyn bv) then Just av else Nothing

mergeWorld wa wb = MkWorld { counter = mergeCnt (counter wa) (counter wb)
                           , store = mergeStore (store wa) (store wb)
                           }

-- | definition a effective impure value
type EffValue = State World PValue


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
eval :: Env Value -> CountRef -> Expr -> Value
eval env cref = go
  where
    recurse = eval env cref
    go (Var name) = fromJust $ M.lookup name env
    go (IntVal i) = VInt i
    go (Add e1 e2) = case (val1, val2) of
      (VInt i1, VInt i2) -> VInt $ i1 + i2
      _                  -> error "Operands of Add isn't VInt value"
      where val1 = recurse e1
            val2 = recurse e2

    go (Mul e1 e2) = case (val1, val2) of
      (VInt i1, VInt i2) -> VInt $ i1 * i2
      _                  -> error "Operands of Mul isn't VInt value"
      where val1 = recurse e1
            val2 = recurse e2

    go (Let vn e body) = eval (M.insert vn (recurse e) env) cref body
    go (IfZero c te fe) = case (recurse c) of
      VInt ci -> if ci == 0 then recurse te
                            else recurse fe
      _       -> error "Condition of IfZero isn't VInt value"

    go (Pair e1 e2) = VPair val1 val2
      where val1 = recurse e1
            val2 = recurse e2

    go (Zro e) = case recurse e of
      VPair l r -> l
      _         -> error "The value of Zro expression isn't VPair value"

    go (Fst e) = case recurse e of
      VPair l r -> r
      _         -> error "The value of Fst expression isn't VPair value"

    go (LeftE e)  = VLeft $ recurse e
    go (RightE e) = VRight $ recurse e

    go (Match e (lv, lb) (rv, rb)) = case recurse e of
      VLeft l  -> eval (M.insert lv l env) cref lb
      VRight r -> eval (M.insert rv r env) cref rb
      _        -> error "The match expression of Match isn't VLeft or VRight value"

    go (Fun v b) = VFun $ \p -> eval (M.insert v p env) cref b

    go (App f x) = case recurse f of
      VFun lam -> lam $ recurse x
      _       -> error "The function of App isn't VFunc value"

    go Count = unsafePerformIO
             $ atomicModifyIORef cref (\c -> let c' = c + 1 in (c', VInt c))

    go Unit = VUnit

    go (MkRef x) = VRef (unsafePerformIO $ newIORef $ recurse x)

    go (GetRef r) = case recurse r of
      VRef ref -> unsafePerformIO $ readIORef ref
      _        -> error "The reference value of GetRef isn't VRef value"

    go (SetRef r x) = case recurse r of
      VRef ref -> unsafePerformIO $ atomicModifyIORef ref
                                      (\_ -> ((recurse x), VUnit))
      _        -> error "The reference value of SetRef isn't VRef value"

-- | A function for partial evaluate expression to PValue
pEval :: Env PValue -> LetlistRef -> Expr -> EffValue
pEval env llref = go
  where
    recurse = pEval env llref
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
          w <- get
          let (le, lw) = withLetList' (\llref ->
                           dynS $ runState (pEval env llref te) w)
          let (re, rw) = withLetList' (\llref ->
                           dynS $ runState (pEval env llref fe) w)
          put $ mergeWorld lw rw
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
          w <- get
          let (le, lw) = withLetList' (\llref ->
                           dynS $ runState (pEval (M.insert lv (mkVarVal lv) env)
                                                  llref lb)
                                           w)
          let (re, rw) = withLetList' (\llref ->
                           dynS $ runState (pEval (M.insert rv (mkVarVal rv) env)
                                                  llref rb)
                                           w)
          put $ mergeWorld lw rw
          return $ mkDynamic llref $ Match s (lv, le) (rv, re)

    go efun@(Fun v b) =
      return $ pFun llref (\llref p -> pEval (M.insert v p env) llref b) efun

    go (App f x) = do
      pf <- recurse f
      px <- recurse x
      case static pf of
        Just (SFun lam) -> lam llref px
        Just  _ -> error "The function value of App expression isn't SFun value"
        Nothing -> do
          put (MkWorld Nothing M.empty)
          return $ mkDynamic llref $ App (Var $ dyn $ pf) (Var $ dyn $ px)

    go Count = do
      w@(MkWorld mc _) <- get
      case mc of
        Just i -> put w{ counter=Just (i + 1) } >> (return $ pInt llref i)
        Nothing -> return $ mkDynamic llref Count

    go Unit = return $ pUnit llref

    go (MkRef x) = do
      px <- recurse x
      let storeId = freshVar ()
      w@(MkWorld _ st) <- get
      put w{ store=M.insert storeId px st }
      return $ mkStatic llref (SRef storeId) (MkRef (Var $ dyn px))

    go (GetRef r) = do
      pr <- recurse r
      case static pr of
        Just (SRef i) -> do
          w@(MkWorld _ st) <- get
          return $ maybe (mkDynamic llref (GetRef (Var $ dyn pr))) id
                 $ M.lookup i st
        Just  _ -> error "The reference id of GetRef expression isn't SRef value"
        Nothing -> return $ mkDynamic llref (GetRef (Var $ dyn pr))

    go (SetRef r x) = do
      pr <- recurse r
      px <- recurse x
      case static pr of
        Just (SRef i) -> do
          w@(MkWorld _ st) <- get
          let st' = M.insert i px st
          put w { store = st' }
          return $ pUnit llref
        Just  _ -> error "The reference id of SetRef expression isn't SInt value"
        Nothing -> return $ mkDynamic llref (SetRef (Var $ dyn pr)
                                                    (Var $ dyn px))

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

pUnit llref = mkStatic llref SUnit Unit

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

pFun llref lam efun = mkStatic llref (SFun lam) (Var "")

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

withLetList' :: (LetlistRef -> (Expr, World)) -> (Expr, World)
withLetList' f = expr `deepseq` (letLetlist ll expr, mc)
  where llref = unsafePerformIO $ newIORef []
        ll = getLetlist llref
        (expr, mc) = f llref

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
