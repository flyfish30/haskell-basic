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
tokNumber s = if hasDot then TokNum (DoubleNum (read (d1 ++ "." ++ d2) :: Double)) : toknize s2
                        else TokNum (IntNum (read d1 :: Int)) : toknize s1
  where (d1, s1) = span isDigit s
        hasDot   = (head s1) == '.'
        (d2, s2) = span isDigit (tail s1)

tokIdent s = TokIdent chars : toknize s1
  where (chars, s1) = span isAlpha s

data Expr a where
  LitI :: Int -> Expr Int
  LitD :: Double -> Expr Double
  Var  :: String -> Expr a
  TermI :: Expr Int -> Expr Int -> Expr Int
  TermD :: Expr Double -> Expr Double -> Expr Double
  Add :: Expr a -> Expr a -> Expr a
  Sub :: Expr a -> Expr a -> Expr a
  Mul :: Expr a -> Expr a -> Expr a
  Div :: Expr a -> Expr a -> Expr a

showExpr :: Expr a -> String
showExpr (LitI i)      = " " ++ show i
showExpr (LitD d)      = " " ++ show d
showExpr (Var s)       = " " ++ s
showExpr (TermI i x)   = " (TermI " ++ showExpr i ++ showExpr x ++ ")"
showExpr (TermD d x)   = " (TermD " ++ showExpr d ++ showExpr x ++ ")"
showExpr (Add e1 e2)   = " (Add " ++ showExpr e1 ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2)   = " (Sub " ++ showExpr e1 ++ showExpr e2 ++ ")"
showExpr (Mul e1 e2)   = " (Mul " ++ showExpr e1 ++ showExpr e2 ++ ")"
showExpr (Div e1 e2)   = " (Div " ++ showExpr e1 ++ showExpr e2 ++ ")"

simplifyExpr :: Expr a -> Expr a
simplifyExpr e@(LitI i)      = e
simplifyExpr e@(LitD d)      = e
simplifyExpr e@(Var s)       = e
simplifyExpr e@(TermI i x)   = e
simplifyExpr e@(TermD d x)   = e
simplifyExpr (Add e1 e2)   = simplifyAddExpr e1 e2
simplifyExpr (Sub e1 e2)   = simplifySubExpr e1 e2
simplifyExpr (Mul e1 e2)   = simplifyMulExpr e1 e2
simplifyExpr (Div e1 e2)   = simplifyDivExpr e1 e2

simplifyAddExpr e1 e2 = undefined
simplifySubExpr e1 e2 = undefined
simplifyMulExpr e1 e2 = undefined
simplifyDivExpr e1 e2 = undefined

hasSameVar :: Expr a -> Expr a -> Bool
hasSameVar (TermI _ (Var s1)) (TermI _ (Var s2)) = s1 == s2
hasSameVar (TermD _ (Var s1)) (TermD _ (Var s2)) = s1 == s2
hasSameVar _ _ = False
