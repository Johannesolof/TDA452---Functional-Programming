module ExprQC where

import Expr
import Test.QuickCheck
import Data.Maybe

-- * Properties

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
