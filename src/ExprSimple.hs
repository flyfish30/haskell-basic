{-# LANGUAGE TypeInType, TypeOperators #-}
{-# LANGUAGE GADTs #-}

module ExprSimple where

import Data.Char
import Data.List

data Token = TokOp Operator
           | TokNum Number
           | TokIdent String
           | TokLParen
           | TokRParen
           | TokEqual
           | TokEnd
           deriving (Show, Eq)

data Operator = TokAdd
              | TokSub
              | TokMul
              | TokDiv
              deriving (Show, Eq)

data Number = IntNum Int
            | DoubleNum Double
            deriving (Eq)

instance Show Number where
  show (IntNum a)    = show a
  show (DoubleNum a) = show a

ops = "+-*/"

toknize :: String -> [Token]
toknize [] = []
toknize s@(c:cs)
  | c `elem` ops = TokOp (tokOperator c) : toknize cs
  | isDigit c    = tokNumber s
  | isAlpha c    = tokIdent s
  | isSpace c    = toknize cs
  | c == '('     = TokLParen : toknize cs
  | c == ')'     = TokRParen : toknize cs
  | c == '='     = TokEqual : toknize cs
  | otherwise = error $ "Can not toknize " ++ [c]

tokOperator :: Char -> Operator
tokOperator c
  | c == '+' = TokAdd
  | c == '-' = TokSub
  | c == '*' = TokMul
  | c == '/' = TokDiv

tokNumber :: String -> [Token]
tokNumber s = if hasDot
              then TokNum (DoubleNum (read (d1 ++ "." ++ d2))) : toknize s2
              else TokNum (IntNum (read d1)) : toknize s1
  where (d1, s1) = span isDigit s
        hasDot   = (head s1) == '.'
        (d2, s2) = span isDigit (tail s1)

tokIdent s = TokIdent chars : toknize s1
  where (chars, s1) = span isAlpha s

data Expr a where
  LitI :: Int -> Expr Int
  LitD :: Double -> Expr Double
  Var  :: String -> Expr a
  TermI :: Int -> Expr Int -> Expr Int
  TermD :: Double -> Expr Double -> Expr Double
  Add :: Expr a -> Expr a -> Expr a
  Sub :: Expr a -> Expr a -> Expr a
  Mul :: Expr a -> Expr a -> Expr a
  Div :: Expr a -> Expr a -> Expr a

showExpr :: Expr a -> String
showExpr (LitI i)      = show i
showExpr (LitD d)      = show d
showExpr (Var s)       = s
showExpr (TermI i x)   = show i ++ showExpr x
showExpr (TermD d x)   = show d ++ showExpr x
showExpr (Add e1 e2)   = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Sub e1 e2)   = showExpr e1 ++ " - " ++ showExpr e2
showExpr (Mul e1 e2)   = showExpr e1 ++ " * " ++ showExpr e2
showExpr (Div e1 e2)   = showExpr e1 ++ " / " ++ showExpr e2

simplifyExpr :: Expr a -> Expr a
simplifyExpr e@(LitI i)    = e
simplifyExpr e@(LitD d)    = e
simplifyExpr e@(Var s)     = e
simplifyExpr e@(TermI i x) = simplifyTermIExpr i $ simplifyExpr x
simplifyExpr e@(TermD d x) = simplifyTermDExpr d $ simplifyExpr x
simplifyExpr e@(Add e1 e2) = simplifyAddExpr (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr e@(Sub e1 e2) = simplifySubExpr (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr e@(Mul e1 e2) = Mul (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr e@(Div e1 e2) = simplifyDivExpr (simplifyExpr e1) (simplifyExpr e2)

simplifyTermIExpr :: Int -> Expr Int -> Expr Int
simplifyTermIExpr i (TermI i1 x) = TermI (i * i1) x
simplifyTermIExpr i (Add e1 e2) = Add (simplifyTermIExpr i e1) (simplifyTermIExpr i e2)
simplifyTermIExpr i (Mul e1 e2) = Mul (TermI i e1) e2
simplifyTermIExpr i e = TermI i e

simplifyTermDExpr :: Double -> Expr Double -> Expr Double
simplifyTermDExpr i (TermD i1 x) = TermD (i * i1) x
simplifyTermDExpr i (Add e1 e2) = Add (simplifyTermDExpr i e1) (simplifyTermDExpr i e2)
simplifyTermDExpr i (Mul e1 e2) = Mul (TermD i e1) e2
simplifyTermDExpr i e = TermD i e

simplifyAddExpr :: Expr a -> Expr a -> Expr a
simplifyAddExpr e1@(TermI a (Var s1)) e2@(TermI b (Var s2))
  | s1 == s2 = TermI (a + b) (Var s1)
  | otherwise = Add e1 e2
simplifyAddExpr e1@(TermD a (Var s1)) e2@(TermD b (Var s2))
  | s1 == s2 = TermD (a + b) (Var s1)
  | otherwise = Add e1 e2
simplifyAddExpr e1 e2 = Add e1 e2

simplifySubExpr :: Expr a -> Expr a -> Expr a
simplifySubExpr e1@(TermI a (Var s1)) e2@(TermI b (Var s2))
  | s1 == s2 = TermI (a + b) (Var s1)
  | otherwise = Add e1 e2
simplifySubExpr e1@(TermD a (Var s1)) e2@(TermD b (Var s2))
  | s1 == s2 = TermD (a + b) (Var s1)
  | otherwise = Add e1 e2
simplifySubExpr e1 e2 = Add e1 e2

simplifyDivExpr :: Expr a -> Expr a -> Expr a
simplifyDivExpr e1@(TermI a (Var s1)) e2@(TermI b (Var s2))
  | s1 == s2 = LitI (a `div` b)
  | otherwise = Div e1 e2
simplifyDivExpr e1@(TermD a (Var s1)) e2@(TermD b (Var s2))
  | s1 == s2 = LitD (a / b)
  | otherwise = Div e1 e2
simplifyDivExpr e1 e2 = Add e1 e2

-- fi for Int TermI, fd for Double TermD
hasSameVar :: Expr a -> Expr a -> Bool
hasSameVar (TermI _ (Var s1)) (TermI _ (Var s2)) = s1 == s2
hasSameVar (TermD _ (Var s1)) (TermD _ (Var s2)) = s1 == s2
hasSameVar _ _ = False
