module BlackJack where
import Cards
import Wrapper
import Test.QuickCheck
--------------- Lab 2A ----------------------------------
-- Group 10 

{- size hand2 = size (Add (Card(Numeric 2) Hearts)
                    (Add (Card Jack Spades) Empty)
        1 + size (Add (Card Jack Spades) Empty)
        1 + 1 + size Empty
        1 + 1 + 0
        2
-}

-- Returns the empty hand.
empty :: Hand
empty = Empty 

-- Gives the total value of a Hand of Cards,
-- taking aces into account.
value :: Hand -> Integer
value h | tot > 21  = tot - (10 * numberOfAces h)
        | otherwise = tot
        where tot = valueHand h
 
-- Gives the value of a Hand where Aces use default value 11. 
valueHand :: Hand -> Integer
valueHand Empty = 0
valueHand (Add c h) = (valueCard c) + (valueHand h) 

-- Gives a value for the input Rank
valueRank :: Rank -> Integer
valueRank Ace = 11
valueRank (Numeric n) = n 
valueRank _ = 10

-- Acquires the value from a given card.
valueCard :: Card -> Integer
valueCard (Card r _) = valueRank r 

-- Returns the amount of aces in the given hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add ( Card Ace _ ) h) = 1 + numberOfAces h 
numberOfAces (Add ( Card _ _ ) h)   = numberOfAces h

-- Returns whether the player is bust'd
gameOver :: Hand -> Bool
gameOver h = value h > 21

-- Compares the player hand to the bank to distinguish the winner.
winner :: Hand -> Hand -> Player
winner p b  | value p > value b = Guest
            | otherwise         = Bank
  
    
----------------Lab 2B----------------------------------------
  

(<+) :: Hand -> Hand -> Hand
h1          <+ Empty        = h1
Empty       <+ h2           = h2
Add c1 h1   <+ h2           = Add c1 (h1 <+ h2)


prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = size (p1 <+ p2) == size p1 + size p2

fullDeck :: Hand 
fullDeck = suitCards Hearts <+ suitCards Clubs
             <+ suitCards Spades <+ suitCards Diamonds

suitCards :: Suit -> Hand
suitCards s1 =  suitNumeric s1 <+ (Add(Card Jack s1)
                                  (Add(Card Queen s1)
                                  (Add(Card King s1) 
                                        Empty)))

suitNumeric :: Suit -> Hand
suitNumeric s1 = suitNumeric1 s1 2

suitNumeric1 :: Suit -> Integer -> Hand 
suitNumeric1 s1 11 = Empty
suitNumeric1 s1 n = Add (Card (Numeric n) s1) Empty 
                    <+ suitNumeric1 s1 (n+1)

