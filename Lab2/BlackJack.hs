module BlackJack where
import Cards
import RunGame

import System.Random
import Test.QuickCheck hiding (shuffle)
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

-- | Returns an empty hand.
empty :: Hand
empty = Empty

-- | Returns the value of a given hand by summarizing the rank of all cards.
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

-- | Determines wether a hand has gone bust.
gameOver :: Hand -> Bool
gameOver h = value h > 21

-- | Determine whose hand will win.
winner :: Hand -> Hand -> Player
winner g b | gameOver g        = Bank
           | gameOver b        = Guest
           | value g > value b = Guest
           | otherwise         = Bank


-- * Part B


-- | OnTopOf: Puts one hand ontop of another hand 
(<+) :: Hand -> Hand -> Hand
(<+) (Add c h1) h2 = Add c (h1 <+ h2)
(<+) Empty (Add c h2) = Add c (Empty <+ h2)
(<+) _ Empty = Empty

-- | Property: Associativity of operator (<+) 
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1<+(p2<+p3) == (p1<+p2)<+p3

-- | Property: The size remain the same
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size (h1 <+ h2) == (size h1 + size h2)

-- | Returns a full deck of 52 cards
fullDeck :: Hand
fullDeck = fullSuit Hearts <+ fullSuit Spades <+ 
           fullSuit Diamonds <+ fullSuit Clubs

-- | Returns a hand of cards with all ranks of one suit
fullSuit :: Suit -> Hand
fullSuit s = buildSuit s Empty
  where
    buildSuit _ h@(Add (Card Ace _) _) = h
    buildSuit s h@(Add (Card King _) _) = 
      buildSuit s (Add (Card Ace s) h)
    buildSuit s h@(Add (Card Queen _) _) = 
      buildSuit s (Add (Card King s) h)
    buildSuit s h@(Add (Card Jack _) _) = 
      buildSuit s (Add (Card Queen s) h)
    buildSuit s h@(Add (Card (Numeric 10) _) _) = 
      buildSuit s (Add (Card Jack s) h)
    buildSuit s h@(Add (Card (Numeric n) _) _) = 
      buildSuit s (Add (Card (Numeric (n + 1)) s) h)
    buildSuit s Empty = 
      buildSuit s (Add (Card (Numeric 2) s) Empty)

-- | Moves the top card of a hand to another hand
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _h    = error "draw: The deck is empty."
draw (Add c d) h = (d, Add c h) 

-- | Draws cards from a deck until the value of the hand exceeds 16 
playBank :: Hand -> Hand
playBank deck = bankDraw (deck, Empty)
  where bankDraw (d, h) | value h >= 16 = h
                        | otherwise     = bankDraw $ draw d h

-- | Shuffles a deck
shuffle :: StdGen -> Hand -> Hand
shuffle g d = shuffler g (d, Empty) (size d)

-- | Helper function for shuffle
shuffler :: StdGen -> (Hand, Hand) -> Integer -> Hand
shuffler g (d1, d2) s | s == 0    = d2
                      | otherwise = shuffler g' (d1', Add c d2) (s-1)
  where (n, g') = randomR (1, s) g
        (c, d1') = removeNth d1 n
        removeNth d = remove d Empty
          where remove Empty _h2 _n = error "removeNth: no deck" 
                remove (Add c h1) h2 i 
                  | i <=1 = (c, h1 <+ h2)
                  | otherwise = remove h1 (Add c h2) (i-1)

-- | Property: The shuffled deck contains the same cards as the 
-- | unshuffled deck
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h

-- | Returns if a card belongs to a hand
belongsTo :: Card -> Hand -> Bool
belongsTo _ Empty = False
belongsTo c (Add c1 h) = (c == c1) || (c `belongsTo` h)

-- | Property: A shuffled deck has the same size as before it was shuffled
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size (shuffle g h) == size h


implementation = Interface
  { iEmpty    = empty
  , iFullDeck = fullDeck
  , iValue    = value
  , iGameOver = gameOver
  , iWinner   = winner 
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffle
  }

-- | Runs the game
main :: IO ()
main = runGame implementation