module ApplicativeParser where

import Data.Char
import Control.Applicative

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

instance Functor Parser where
    -- | Change the result of a parser.
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P $ \s -> map (\(s1, a) -> (s1, f a)) $ unP p s

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P $ \s -> case s of
                      []                 -> []
                      (c:cs) | p c       -> [(cs, c)]
                             | otherwise -> []

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP x = predP (== x)

instance Applicative Parser where
    -- | Inject a value into an identity parser.
    -- pure :: a -> Parser a
    pure x = P $ \s -> [(s, x)]

    -- | Given a parser with a function value and another parser, parse the function
    -- first and then the value, return a parser which applies the function to the
    -- value.
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> px = P $ \s -> concat [[(s2, f a) | (s2, a) <- unP px s1] | (s1, f) <- unP pf s]

-- | Parse a whole string.
stringP :: String -> Parser String
stringP [] = P $ \s -> [(s, [])]
stringP (c:cs) = (:) <$> charP c <*> stringP cs

instance Alternative Parser where
    -- | Construct a parser that never parses anything.
    -- empty :: Parser a
    empty = P $ \s -> []

    -- | Combine two parsers: When given an input, provide the results of both parser
    -- run on the input.
    -- (<|>) :: Parser a -> Parser a -> Parser a
    p1 <|> p2 = P $ \s -> unP p1 s ++ unP p2 s

-- | Apply the parser zero or more times.
-- many :: Parser a -> Parser [a]
-- many p = some p <|> pure []

-- | Apply the parser one or more times.
-- some :: Parser a -> Parser [a]
-- some p = (:) <$> p <*> many p


-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = map snd . filter (null . fst) $ unP p cs

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs = getResult $ runParser p cs
  where getResult [a] = Just a
        getResult _   = Nothing

-- | Kinds of binary operators.
data BinOp = AddBO | SubBO
           | MulBO | DivBO
           | ExpBO
           deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE  Int
          | BinOpE  BinOp Expr Expr
          | NegE    Expr
          | ZeroE
          deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr (ConstE i) = i
evalExpr (BinOpE op e1 e2)
  | op == AddBO     = evalExpr e1 + evalExpr e2
  | op == SubBO     = evalExpr e1 - evalExpr e2
  | op == MulBO     = evalExpr e1 * evalExpr e2
  | op == DivBO     = evalExpr e1 `div` evalExpr e2
  | op == ExpBO     = evalExpr e1 ^ evalExpr e2
  | otherwise       = error $ "Not support operator " ++ show op
evalExpr (NegE e)   = negate $ evalExpr e
evalExpr ZeroE      = 0

-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= subExpr | binOpExpr
--     subExpr      ::= '(' expr ')' | const | neg | zero
--     binOpExpr    ::= sumOpExpr | prodOpExpr | expOpExpr
--     sumOpExpr    ::= '(' subExpr ' ' sumOp  ' 'subExpr ')'
--     sumOp        ::= '+' | '-'
--     prodOpExpr   ::= '(' subExpr ' ' prodOp ' 'subExpr ')'
--     prodOp       ::= '*' | '/'
--     expOpExpr    ::= '(' subExpr ' ' expOp  ' 'subExpr ')'
--     expOp        ::= '^'
--     const        ::= int
--     neg          ::= '-' subExpr
--     zero         ::= 'z'
-- 
parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique exprP

exprP :: Parser Expr
exprP = subExprP <|> binOpExprP

subExprP = charP '(' *> exprP <* charP ')' <|> constP <|> negP <|> zeroP

binOpExprP = sumOpExprP <|> prodOpExprP <|> expOpExprP

sumOpExprP = ((flip BinOpE) <$> (subExprP <|> prodOpExprP) <* spaceP
                            <*> sumOpP <* spaceP
                            <*> (subExprP <|> sumOpExprP <|> prodOpExprP))

sumOpP = AddBO <$ charP '+' <|> SubBO <$ charP '-'

prodOpExprP = (flip BinOpE) <$> (subExprP <|> expOpExprP) <* spaceP
                            <*> prodOpP <* spaceP
                            <*> (subExprP <|> prodOpExprP <|> expOpExprP)

prodOpP = MulBO <$ charP '*' <|> DivBO <$ charP '/'

expOpExprP = (flip BinOpE) <$> subExprP <* spaceP
                           <*> expOpP <* spaceP
                           <*> subExprP

expOpP = ExpBO <$ charP '^'

constP = (ConstE . read) <$> some digitP

negP  = NegE <$ charP '-' <*> subExprP

zeroP = const ZeroE <$> charP 'z'

digitP = predP isDigit

spaceP = charP ' '

