module Expr where

import GHC.Unicode

import Parsing

-- * Assignment A

data Expr =   EVar
            | ENum  Double
            | EOp   Expr Op Expr
            | EFun  Fun Expr

data Op   = Add | Mul
  deriving (Enum, Eq)

-- Returns first element in datatype that can be used in list comprehension
fstOp :: Op
fstOp = Add

-- Custom ordering for defining precedence
instance Ord Op where
    compare Mul Mul = EQ
    compare Add Add = EQ
    compare Mul Add = GT
    compare Add Mul = LT

data Fun  = Sin | Cos
  deriving Enum

-- Returns first element in datatype that can be used in list comprehension
fstFun :: Fun
fstFun = Sin



-- * Assignment B

instance Show Expr where
  show = showExpr

instance Show Op where
  show = showOp

instance Show Fun where
  show = showFun

-- Encloses a string in paranthesis
enclose :: String -> String
enclose s = concat ["(", s, ")"]


showOp :: Op -> String
showOp Add = " + "
showOp Mul = " * "

showFun :: Fun -> String
showFun Sin = "sin "
showFun Cos = "cos "

showExpr :: Expr -> String
showExpr  EVar          = "x"
showExpr (ENum n)       = show n
showExpr (EOp e1 op e2) = concat [showEOp e1 op, show op, showEOp e2 op]
showExpr (EFun f e)     = show f ++ showEFun e

-- Encloses expression if needed depending on precedence
-- Note:  c = child,   p = parent
showEOp :: Expr -> Op -> String
showEOp (EOp e1 c e2) p | p `prec` c = enclose $ showExpr (EOp e1 c e2)
showEOp e _                          = showExpr e

-- Encloses expression if needed depending on sub expression
showEFun :: Expr -> String
showEFun (EOp e1 op e2) = enclose $ showExpr (EOp e1 op e2)
showEFun  e             = showExpr e

-- Returns whether first op has higher precedence than second op
prec :: Op -> Op -> Bool
prec op1 op2 = op1 > op2


-- * Assignment C

eval :: Expr -> Double -> Double
eval  EVar v            = v
eval (ENum d) _v        = d
eval (EOp e1 Mul e2) v  = eval e1 v * eval e2 v
eval (EOp e1 Add e2) v  = eval e1 v + eval e2 v
eval (EFun Sin e) v     = sin $ eval e v
eval (EFun Cos e) v     = cos $ eval e v


-- * Assignment D

{-
< expression > ::= < term > + < expression > | < term >
< term > ::= < factor > * < term > | < factor >
< function > ::= < id > < factor >
< id > ::= sin | cos
< factor > ::= < function > | (< expression >) | < double > | < var >
-}

var :: Parser Expr
var = do _ <- char 'x'
         return EVar
      <|>
      do _ <- char '-'
         _ <- char 'x'
         return $ EOp (ENum (-1)) Mul EVar

num :: Parser Expr
num = do d <- readsP
         return $ ENum d

string :: String -> Parser String
string [] = return []
string (c:cs) = do char c
                   string cs
                   return (c:cs)

fun :: Parser Fun
fun = do string "sin"
         return Sin
      <|>
      do string "cos"
         return Cos


function :: Parser Expr
function = do fu <- fun
              f <- factor
              return $ EFun fu f

expr :: Parser Expr
expr = do t <- term
          _ <- char '+'
          e <- expr
          return $ EOp t Add e
        <|> term

term :: Parser Expr
term  = do f <- factor
           _ <- char '*'
           t <- term
           return $ EOp f Mul t
        <|> factor

factor :: Parser Expr
factor = function <|>
         do _ <- char '('
            e <- expr
            _ <- char ')'
            return e
         <|> var <|> num


readExpr :: String -> Maybe Expr
readExpr s = case parse expr s' of
                  Just (e,"") -> Just e
                  _           -> Nothing
  where s' = filter (not . isSpace) s

-- * Assignment F

simplify :: Expr -> Expr
simplify (EOp e1 Add e2) = case (simplify e1, simplify e2) of
                                (ENum d1, ENum d2) -> ENum (d1 + d2)
                                (se1, ENum 0)      -> se1
                                (ENum 0, se2)      -> se2
                                (se1, se2)         -> EOp se1 Add se2
simplify (EOp e1 Mul e2) = case (simplify e1, simplify e2) of
                                (ENum d1, ENum d2) -> ENum (d1 * d2)
                                (ENum 0, _)        -> ENum 0
                                (_, ENum 0)        -> ENum 0
                                (se1, ENum 1)      -> se1
                                (ENum 1, se2)      -> se2
                                (se1, se2)         -> EOp se1 Mul se2
simplify (EFun Sin e1) = case simplify e1 of
                              ENum d -> ENum (sin d)
                              se1    -> EFun Sin se1
simplify (EFun Cos e1) = case simplify e1 of
                              ENum d -> ENum (cos d)
                              se1    -> EFun Cos se1
simplify e = e


-- * Assignment G
differentiate :: Expr -> Expr
differentiate = simplify . differentiate'

differentiate' :: Expr -> Expr
differentiate' EVar = ENum 1
differentiate' (ENum _) = ENum 0
differentiate' (EOp e1 Add e2) = EOp (differentiate e1) Add
                                     (differentiate e2)
differentiate' (EOp e1 Mul e2) = EOp (EOp (differentiate e1) Mul e2) Add
                                     (EOp e1 Mul (differentiate e2))
differentiate' (EFun Sin e) = EOp (differentiate e) Mul (EFun Cos e)
differentiate' (EFun Cos e) = EOp (differentiate e) Mul
                                  (EOp (ENum (-1)) Mul (EFun Sin e))
