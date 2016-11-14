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
gameOver h = value h <= 21

-- | Determine whose hand will win.
winner :: Hand -> Hand -> Player
winner g b | gameOver g        = Bank
           | gameOver b        = Guest
           | value g > value b = Guest
           | otherwise         = Bank

-- * Part B

hand1 = Add (Card (Numeric 5) Spades) Empty
hand2 = Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)
hand3 = Add (Card Ace Hearts) (Add (Card (Numeric 4) Spades) (Add (Card Queen Diamonds) Empty))

(<+) :: Hand -> Hand -> Hand
(<+) (Add c h1) h2 = Add c (h1 <+ h2)
(<+) Empty (Add c h2) = Add c (Empty <+ h2)
(<+) _ Empty = Empty

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size (h1 <+ h2) == (size h1 + size h2)

fullDeck :: Hand
fullDeck = fullSuit Hearts <+ fullSuit Spades <+ 
           fullSuit Diamonds <+ fullSuit Clubs

fullSuit :: Suit -> Hand
fullSuit s = buildSuit s Empty

buildSuit :: Suit -> Hand -> Hand
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

draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _h    = error "draw: The deck is empty."
draw (Add c d) h = (d, Add c h) 

playBank :: Hand -> Hand
playBank deck = bankDraw (deck, Empty)
  where bankDraw (d, h) | value h >= 16 = h
                        | otherwise     = bankDraw $ draw d h
                        
