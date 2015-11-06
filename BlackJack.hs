module BlackJack where
import Cards
import Wrapper

{- size hand2 = size (Add (Card(Numeric 2) Hearts)
                    (Add (Card Jack Spades) Empty)
        1 + size (Add (Card Jack Spades) Empty)
        1 + 1 + size Empty
        1 + 1 + 0
        2
-}

empty :: Hand
empty = Empty 

hand1 = (Add(Card(Numeric 2) Hearts)(Add (Card Jack Spades) Empty))
hand2 = (Add(Card(Numeric 3) Hearts)(Add (Card Jack Hearts) Empty))
acehand = (Add(Card Ace Hearts)(Add(Card Ace Clubs)(Add(Card Ace Diamonds)(Add(Card Ace Spades) Empty))))

-- case number of ace
value :: Hand -> Integer
value h | valueHand h > 21  = valueHand h -10*numberOfAces h
        | otherwise = valueHand h
                
valueHand :: Hand -> Integer
valueHand Empty = 0
valueHand (Add c h) = (valueCard c) + (valueHand h) 
                
valueRank :: Rank -> Integer
valueRank King = 10
valueRank Queen = 10
valueRank Jack = 10
valueRank (Numeric n) = n 
valueRank _ = 11

-- valueCard acquires the value based on the card rank
-- and Aces will either return one or 11.
valueCard :: Card -> Integer
valueCard (Card r _) = valueRank r 

numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add ( Card Ace _ ) h) = 1 + numberOfAces h 
numberOfAces (Add ( Card _ _ ) h)   = numberOfAces h

gameOver :: Hand -> Bool
gameOver h = value h > 21

winner :: Hand -> Hand -> Player
winner p b  | value p > value b = Guest
            | otherwise         = Bank