module Lab1 where

import Test.QuickCheck


power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k - 1)


{-----Part 1------}

{-
k + 1 "steps" are being used.
-}

{-----Part 2------}

power1 :: Integer -> Integer -> Integer
power1 _ k | k < 0 = error "power: negative argument"
power1 n k = product [n | _ <- [1 .. k]]

{-----Part 3------}

power2 :: Integer -> Integer -> Integer
power2 _ k | k < 0 = error "power: negative argument"
power2 _ 0 = 1
power2 n k | even k = power2 (n * n) (div k 2) 
power2 n k = n * power2 n (k-1)


{-----Part 4------}

{-A-}
{-Test cases: no test case where k is lesser than 0 since the functions are not defined then. -}
testCases :: [(Integer, Integer)]
testCases = [(0,0), (6,0), (0,11), (32, 54), (43, 21)]

{-B-}
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = a1==a2 && a2==a3 
    where a1 = power n k
          a2 = power1 n k
          a3 = power2 n k

{-C-}
powerTestCases :: Bool
powerTestCases = and [ prop_powers n k | (n, k) <- testCases]

{-D-}
prop_powers' :: Integer -> Integer -> Property
prop_powers' n k = k >= 0 ==> a1==a2 && a2==a3 
    where a1 = power n k
          a2 = power1 n k
          a3 = power2 n k
