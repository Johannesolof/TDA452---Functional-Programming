module BlackJack where
import Cards
import RunGame



{-Task 3.0-}
{-
size hand2
  = size (Add (Card (Numeric 2) Hearts)
              (Add (Card Jack Spades) Empty))
  = 1 + size (Add (Card Jack Spades) Empty)
  = 1 + 1 + size Empty
  = 2 + 0
  = 2
-}


{-Task 3.1-}

-- Returns an empty hand
empty :: Hand
empty = Empty

-- Calculates the value of a given hand, 
-- where the value is the sum of all cards rank
value :: Hand -> Integer
value Empty = 0
value (Add (Card (Numeric v) _) h) = v + value h
value (Add (Card _ _) h) = 10 + value h

gameOver :: Hand -> Bool
gameOver _ = undefined

winner :: Hand -> Hand -> Player
winner _ _ = undefined