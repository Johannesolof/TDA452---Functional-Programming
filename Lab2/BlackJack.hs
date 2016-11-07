module BlackJack where
import Cards
import RunGame



{-Task 3.1-}
{-
size hand2
  = size (Add (Card (Numeric 2) Hearts)
              (Add (Card Jack Spades) Empty))
  = 1 + size (Add (Card Jack Spades) Empty)
  = 1 + 1 + size Empty
  = 2 + 0
  = 2
-}


{-Task 3.4-}

-- Returns an empty hand
empty :: Hand
empty = Empty

-- Calculates the value of a given hand, 
-- where the value is the sum of all cards rank
value :: Hand -> Integer
value h = if v > 21 then v - 10 * a else v
    where (v, a) = valueHelper h

-- Helper function for value, keeping track of the number of aces in the hand
valueHelper :: Hand -> (Integer, Integer)
valueHelper Empty = (0, 0)
valueHelper (Add (Card (Numeric n) _) h) = (v + n, a)
    where (v, a) = valueHelper h
valueHelper (Add (Card Ace _) h) =  (v + 11, a + 1)
    where (v, a) = valueHelper h
valueHelper (Add (Card _ _) h) = (v + 10, a)
    where (v, a) = valueHelper h

-- Tells if a hand is busted
gameOver :: Hand -> Bool
gameOver h | value h <= 21 = False
gameOver _ = True

-- Determine whose hand will win
winner :: Hand -> Hand -> Player
winner g _ | gameOver g = Bank
winner _ b | gameOver b = Guest
winner g b | value g > value b = Guest
winner _ _ = Bank