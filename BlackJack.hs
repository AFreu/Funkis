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
  
-- Append operator which works for hands
(<+) :: Hand -> Hand -> Hand
h1          <+ Empty        = h1
Empty       <+ h2           = h2
Add c1 h1   <+ h2           = Add c1 (h1 <+ h2)


prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = size (p1 <+ p2) == size p1 + size p2

-- Returns a full deck of 52 cards ordered by size and suit.
fullDeck :: Hand 
fullDeck = suitCards Hearts <+ suitCards Clubs
             <+ suitCards Spades <+ suitCards Diamonds

-- Returns all cards of given suit
suitCards :: Suit -> Hand
suitCards s1 =  suitNumeric s1 <+ Add(Card Jack s1)
                                  (Add(Card Queen s1)
                                  (Add(Card King s1)
                                  (Add(Card Ace s1)
                                        Empty)))
-- Returns all numeric cards given a suit.
suitNumeric :: Suit -> Hand
suitNumeric s1 = suitNumeric' s1 2

-- Helper function to recursively add all numbered cards.
suitNumeric' :: Suit -> Integer -> Hand 
suitNumeric' s1 11 = Empty
suitNumeric' s1 n = Add (Card (Numeric n) s1) Empty 
                    <+ suitNumeric' s1 (n+1)

-- Draw a card from the deck into the hand.
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty  h       = error "draw: The deck is empty."
draw (Add c d) h    = (d, Add c h)

-- Draws cards for the bank. The bank stops when the sum of its cards
-- is above 15.
playBank :: Hand -> Hand
playBank deck = playBank' (deck, Empty) 

-- Helper function
playBank' :: (Hand, Hand) -> Hand
playBank' (deck,bank) | value bank > 15  = bank
                      | otherwise        = playBank' (deck',bank')
                where (deck',bank') = draw deck bank

-- Shuffles the deck
shuffle :: StdGen -> Hand -> Hand       
shuffle _ Empty = Empty
shuffle gen deck = Add card (shuffle gen1 deck1)
            where (n, gen1) = randomR (0, size deck - 1) gen
                  Add card deck1 = moveCard deck n 

--Puts the card of index n on top of the hand.
moveCard :: Hand -> Integer -> Hand
moveCard Empty _ = error "index out of bounds"
moveCard deck 0 = deck  
moveCard (Add card deck) n = moveCard deck (n-1) <+ Add card Empty 

--Tests whether the cards are the same before and after shuffle.
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
