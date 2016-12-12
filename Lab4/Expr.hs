module Expr where

import Test.QuickCheck

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
showExpr (EFun f e)     = concat [show f, spc, showEnclose e Mul]
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


-- * Assignment C

readExpr :: String -> Maybe Expr
readExpr s = undefined


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
