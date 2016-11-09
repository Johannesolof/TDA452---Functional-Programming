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
{-

Test cases: 

P : (0,0)
Q : (32, 54)
R : (-43, 22)
S : (32, 53)
T : (-43, 21)

Motivation: 

We do not have any test cases where 'k' is smaller than zero since our 
functions are not defined for this. We defined test case 'P' to test the 
smallest value of k (edge case) and the matching patterns where 'k' is zero in
'power' and 'power2'. For all the defined power functions we also want to 
ensure that they compute the correct result for both positive and negative 
values. To test this we define 'Q' with an arbitrary postive value for 'n' and
'R' with and arbitray negative value for 'n'.

Finally, in 'power2' we have different matching cases for even and uneven
values of 'k' that we need to test. We already test the case where 'k' is even
with both positive and negative values of 'n' in our test cases 'Q' and 'R'.
What remains is to test both positive and negative values but with an uneven
value of 'k'. This is achieved with test cases 'S' and 'T'.


-}
testCases :: [(Integer, Integer)]
testCases = [(0,0), (32,54), (-43,22), (32, 53), (-43, 21)]

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
prop_powers' n k = k >= 0 ==> prop_powers n k
