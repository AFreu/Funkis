module BlackJack where
import Cards
import Wrapper
import Test.QuickCheck
    hiding (shuffle)
import System.Random
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
winner p b  | gameOver p = Bank
            | value p > value b || gameOver b = Guest 
            | otherwise  = Bank
  
    
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
                                  (Add(Card Ace s1)
                                        Empty))))

suitNumeric :: Suit -> Hand
suitNumeric s1 = suitNumeric' s1 2

suitNumeric' :: Suit -> Integer -> Hand 
suitNumeric' s1 11 = Empty
suitNumeric' s1 n = Add (Card (Numeric n) s1) Empty 
                    <+ suitNumeric' s1 (n+1)

draw :: Hand -> Hand -> (Hand,Hand)
draw Empty  h       = error "draw: The deck is empty."
draw (Add c d) h    = (d, Add c h)

playBank :: Hand -> Hand
playBank d = playBank' (d, Empty) 

playBank' :: (Hand, Hand) -> Hand
playBank' (d,b) | value b > 15  = b
                | otherwise     = playBank' (d',b')
                    where (d',b') = draw d b

shuffle :: StdGen -> Hand -> Hand
shuffle g d  = shuffle' g (d,Empty)   

shuffle' :: StdGen -> (Hand, Hand) -> Hand
shuffle' g (Empty,shufDeck) = shufDeck
shuffle' g (deck,shufDeck)  = shuffle' g' (newDeck,(Add c shufDeck)) 
    where   (n,g')       = randomR (0, (size deck)-1) g
            (c, newDeck) = indexDraw (deck, Empty) n
       

-- Returns the hand without the card of given index,
-- and the card itself.
indexDraw :: (Hand,Hand) -> Integer -> (Card, Hand)
indexDraw (Empty, d') _     = error "index not present" 
indexDraw ((Add c d), d') 0 = (c, d' <+ d)
indexDraw ((Add c d), d') n  = indexDraw (d ,(Add c d')) (n-1)

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo`(Add c' h) = c == c' || c `belongsTo` h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffle g h)

implementation = Interface
    {iEmpty         = empty
    ,iFullDeck      = fullDeck
    ,iValue         = value
    ,iGameOver      = gameOver
    ,iWinner        = winner
    ,iDraw          = draw
    ,iPlayBank      = playBank
    ,iShuffle       = shuffle
    }
    
main :: IO ()
main = runGame implementation
