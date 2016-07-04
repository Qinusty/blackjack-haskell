{-# LANGUAGE UnicodeSyntax #-}

module Cards where

import Data.List (intercalate)
import Data.Random hiding (shuffle)
import Data.Random.Extras
data Suit = Spades | Clubs | Diamonds | Hearts deriving (Show, Enum, Eq)
data Value = Ace | Two | Three | Four | Five | Six | Seven
            | Eight | Nine | Ten | Jack | Queen | King 
            deriving (Show, Enum, Eq)

data Card = Card Suit Value | Joker deriving (Eq)
type Deck = [Card]
type Hand = [Card]

instance Show Card where
    show (Card suit val) = show val ++ " Of " ++ show suit
    show Joker = "Joker"

compareHand :: Hand -> Hand -> Ordering
compareHand hand1 hand2
    | highestValidHandValue values1 >= highestValidHandValue values2 = GT
    | otherwise = LT
    where 
        values1 = valueOfHand hand1
        values2 = valueOfHand hand2


highestValidHandValue :: [Int] -> Int
highestValidHandValue = (\xs -> if not $ null xs then maximum xs else -1) . filter (<= 21)

newDeck :: Deck
newDeck = [Card suit val | suit <- [Spades ..], val <- [Ace ..]]

printHand :: Hand -> IO()
printHand hand = putStrLn $ showHand hand

showHand :: Hand -> String
showHand hand = (++ (" With possible value(s) of " ++ intercalate ", "
                             (map show $ valueOfHand hand))) $ intercalate ", " $ 
                                map show hand


valueOfHand :: Hand -> [Int]
valueOfHand hand = map sum $ mapM id listOfCardValues
    where listOfCardValues = map valueOfCard hand

isBust :: Hand -> Bool
isBust hand = lowestVal > 21
    where lowestVal = minimum $ valueOfHand hand

valueOfCard :: Card -> [Int]
valueOfCard Joker          = [0]
valueOfCard (Card _ Ace)   = [1, 11]
valueOfCard (Card _ n)     = [k] 
    where k = min (1 + fromEnum n) 10

drawCard :: Deck -> (Card, Deck)
drawCard [] = error "Game over, Somehow your deck is out of cards."
drawCard (c:cs) = (c, cs)

drawCards :: Int -> Deck -> ([Card], Deck)
drawCards _ [] = error "Empty Deck Sorry" 
drawCards 0 d = ([], d)
drawCards n (c:cs) = let (cards,deck') = drawCards (n-1) cs in (c:cards, deck') 

shuffleDeck :: MonadRandom m => Deck -> m Deck
shuffleDeck deck = runRVar (shuffle deck) StdRandom
