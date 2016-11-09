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

-- | Returns an empty hand
empty :: Hand
empty = Empty

-- | Returns the value of a given hand by summarizing the rank of all
-- | cards.
value :: Hand -> Integer
value h = if v > 21 then v - 10 * numberOfAces h else v
    where v = sumHand h

-- | Returns the value of a card.
valueCard :: Card -> Integer
valueCard c = valueRank $ rank c

-- | Returns the value of a rank.
valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Ace = 11
valueRank _ = 10

-- | Returns the number of aces in a given hand.
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace _s) h) = 1 + numberOfAces h
numberOfAces (Add _c h) = numberOfAces h


-- | Sums the value of each card in a hand.
sumHand :: Hand -> Integer
sumHand Empty = 0
sumHand (Add c h) = valueCard c + sumHand h

-- Determines wether a hand has gone bust.
gameOver :: Hand -> Bool
gameOver h | value h <= 21 = False
gameOver _ = True

-- Determine whose hand will win.
winner :: Hand -> Hand -> Player
winner g _ | gameOver g = Bank
winner _ b | gameOver b = Guest
winner g b | value g > value b = Guest
winner _ _ = Bank