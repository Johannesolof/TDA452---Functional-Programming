module Expr where

import Test.QuickCheck
import GHC.Unicode
import Data.Maybe

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

-- Returns a blank space as string
spc :: String
spc = " "

-- Encloses a string in paranthesis
enclose :: String -> String
enclose s = concat ["(", s, ")"]


showOp :: Op -> String
showOp Add = " + "
showOp Mul = " * "

showFun :: Fun -> String
showFun Sin = "sin"
showFun Cos = "cos"

showExpr :: Expr -> String
showExpr  EVar          = show "x"
showExpr (ENum n)       = show n
showExpr (EOp e1 op e2) = concat [showEnclose e1 op, show op, showEnclose e2 op]
showExpr (EFun f e)     = concat [show f, spc, enclose (showExpr e)]
-- Enclose fun expression if precedence is lower than mul

-- Encloses expression if needed depending on precedence
-- Note:  c = child,   p = parent
showEnclose :: Expr -> Op -> String
showEnclose (EOp e1 c e2) p | p `prec` c = enclose $ showExpr (EOp e1 c e2)
showEnclose e _                          = showExpr e

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

double :: Parser Double
double = do ds <- oneOrMore digit
            _ <- char '.'
            dcs <- oneOrMore digit
            return $ read ds + read dcs / (10 ^ length dcs)
         <|> 
         do ds <- oneOrMore digit
            return $ read ds

num :: Parser Expr
num = do _ <- char '-'
         d <- double
         return $ ENum (negate d)
      <|>
      do d <- double
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
function = do fun <- fun
              f <- factor
              return $ EFun fun f

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

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = show e == show e'
  where e' = fromJust $ readExpr $ show e


-- * Expression generators

-- Reduces level, used to control size of samples
rdcSize :: Int -> Int
rdcSize s = s `div` 2

rExpr :: Int -> Gen Expr
rExpr s = frequency [(1, rNum), (s, rBin s), (s, rFun s)]

rNum :: Gen Expr
rNum = elements $ map ENum [-range..range]
  where range = 10

rBin :: Int -> Gen Expr
rBin s = do
  let s' = rdcSize s
  op <- elements [fstOp..]
  e1 <- rExpr s'
  e2 <- rExpr s'
  return $ EOp e1 op e2

rFun :: Int -> Gen Expr
rFun s = do
  let s' = rdcSize s
  f <- elements [fstFun..]
  e <- rExpr s'
  return $ EFun f e

instance Arbitrary Expr where
  arbitrary = sized rExpr