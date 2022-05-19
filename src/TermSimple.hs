module ExprSimple where

import Data.Char
import Data.List
import Data.Function

-- | the polynom lis is little endian mode, [x^0, x^1, x^2..]
newtype BinaryPolynom = BPoly { unPoly :: [Int] }
                        deriving (Eq)

zero, one :: BinaryPolynom
zero = BPoly []
one  = BPoly [1]

deg :: BinaryPolynom -> Int
deg (BPoly []) = -1
deg (BPoly ps) = length ps - 1

-- | Constructs a monom with the given degree.
polyFromDeg :: Int -> BinaryPolynom
polyFromDeg n | n == (-1) = BPoly []
              | n >= 0    = BPoly (psShiftR n [1])
              | otherwise = error "Invalid degree of polynom"

polyFromPowers :: [Int] -> BinaryPolynom
polyFromPowers [] = BPoly []
polyFromPowers ts = BPoly . foldr1 (zipMWith (+))
                  $ map (unPoly . polyFromDeg) ts

instance Show BinaryPolynom where
    show (BPoly []) = "0"
    show (BPoly ps) = concat . intersperse " + " . map showTerm . filter ((/=0) . fst)
                    $ zip (reverse ps) [n, n-1..0]
      where showTerm (c, 0) = "1"
            showTerm (c, e) = "x^" ++ show e
            n = length ps - 1

{-# INLINE psShiftL #-}
psShiftL n ps = ps ++ replicate n 0

{-# INLINE psShiftR #-}
psShiftR n ps = replicate n 0 ++ ps

{-# INLINE psClean0Tails #-}
psClean0Tails = reverse . dropWhile (== 0) . reverse

{-# INLINE addMod2 #-}
addMod2 a b = (a + b) `rem` 2

-- add two polynom list in little endian mode
{-# INLINE psAddLE #-}
psAddLE = zipMWith addMod2

-- | Addition in the polynom ring.
addition :: BinaryPolynom -> BinaryPolynom -> BinaryPolynom
addition (BPoly ps1) (BPoly ps2) = BPoly $ psClean0Tails $ psAddLE ps1 ps2

-- | Multiplication in the polynom ring.
multiply :: BinaryPolynom -> BinaryPolynom -> BinaryPolynom
multiply (BPoly ps1) (BPoly ps2)
  | null ps1  = BPoly []
  | null ps2  = BPoly []
  | otherwise = BPoly . foldr1 psAddLE
              . map (flip psShiftR ps1 . snd)
              . filter ((/=0) . fst) $ zip ps2 [0..n]
  where n = length ps2 - 1

-- | Addition and multiplication in the polynom field.
(.+.), (.*.) :: BinaryPolynom -> BinaryPolynom -> BinaryPolynom
x .+. y = snd $ polyDivMod (BPoly $ psClean0Tails $ psAddLE (unPoly x) (unPoly y))
                           irrePolyF2e8

x .*. y = snd $ polyDivMod (multiply x y) irrePolyF2e8 

{-# INLINE irrePolyF2e8 #-}
irrePolyF2e8 = polyFromPowers [8, 4, 3, 1, 0]

polyDivMod :: BinaryPolynom -> BinaryPolynom -> (BinaryPolynom, BinaryPolynom)
polyDivMod x y = let (qs, rs) = go ([], unPoly x) (unPoly y) in (BPoly qs, BPoly rs)
  where
    go (psq, ps1) ps2 = if n1 >= n2
                        then go (psAddLE psq (psShiftR (n1 - n2) [1]),
                                 psClean0Tails $ psAddLE ps1 (psShiftR (n1 - n2) ps2))
                                ps2
                        else (psq, ps1)
      where n1 = length ps1
            n2 = length ps2

zipMWith :: (a -> a -> a) -> [a] -> [a] -> [a]
zipMWith f [] ys = ys
zipMWith f xs [] = xs
zipMWith f (x:xs) (y:ys) = f x y : zipMWith f xs ys

simplify :: String -> String
simplify = showExpr . simplifyExpr . parseExpr

type Vars = String

data Term = TermNil
          | Term Int Vars
          deriving (Eq)

instance Show Term where
  show TermNil     = ""
  show (Term i []) = show i
  show (Term i vars)
    | i ==   1  = vars
    | i == (-1) = "-" ++ vars
    | otherwise = show i ++ vars

instance Ord Term where
  compare TermNil _ = GT
  compare _ TermNil = LT
  compare (Term i1 vars1) (Term i2 vars2)
    | l1 == l2  = compare vars1 vars2
    | otherwise = compare l1 l2
    where l1 =length vars1
          l2 =length vars2

instance Semigroup Term where
  (<>) = addTerm

instance Monoid Term where
  mempty  = TermNil
  mappend = addTerm

hasSameVars :: Term -> Term -> Bool
hasSameVars (Term _ vars1) (Term _ vars2) = vars1 == vars2
hasSameVars _ _ = False

addTerm :: Term -> Term -> Term
addTerm (Term i1 vars1) (Term i2 vars2)
  | vars1 == vars2 = Term (i1 + i2) vars1
  | otherwise      = error "Can not add together with difference vars"
addTerm TermNil t = t
addTerm t TermNil = t

showExpr :: [Term] -> String
showExpr []  = ""
showExpr (t1:ts)
  | needPlusSymb ts = show t1 ++ "+" ++ showExpr ts
  | otherwise       = show t1 ++ showExpr ts
  where needPlusSymb ts
          | null ts = False
          | otherwise = isTermPositive $ head ts

simplifyExpr :: [Term] -> [Term]
simplifyExpr = filter (not . isZeroTerm)
             . map (foldr mappend TermNil) . groupBy hasSameVars . sort

isZeroTerm :: Term -> Bool
isZeroTerm TermNil       = True
isZeroTerm (Term i vars) = i == 0

isTermPositive :: Term -> Bool
isTermPositive TermNil       = True
isTermPositive (Term i vars) = if i < 0 then False else True

parseExpr :: String -> [Term]
parseExpr s = if t == TermNil then [] else t : parseExpr rs
  where (t, rs) = parseTerm s

parseTerm :: String -> (Term, String)
parseTerm [] = (TermNil, [])
parseTerm s@(c:cs)
  | isSpace c     = parseTerm cs
  | c `elem` "+-" = let (term, cs') = parseTerm cs
                    in (signTerm c term, cs')
  | isDigit c     = let (num, cs') = parseNum s
                        (vars, cs'') = parseVars cs'
                    in (Term num vars, cs'')
  | isAlpha c     = let (vars, cs') = parseVars s
                    in (Term 1 vars, cs')
  | otherwise     = error $ "Leftover string: " ++ s

parseNum :: String -> (Int, String)
parseNum [] = error "Expected digit but nothing"
parseNum s = (read digits, cs)
  where (digits, cs) = span isDigit s

parseVars :: String -> (Vars, String)
parseVars [] = ("", [])
parseVars s = (sort vars, cs)
  where (vars, cs) = span isAlpha s

signTerm :: Char -> Term -> Term
signTerm c t
  | t /= TermNil = if c == '-' then negTerm t else t
  | otherwise    = error "Expected term but nothing for signTerm"
  where negTerm (Term i vars) = Term (0 - i) vars

drinkBeer 0 a = a
drinkBeer n a = drinkBeer (n - 1) ((a + 1) / 2)
