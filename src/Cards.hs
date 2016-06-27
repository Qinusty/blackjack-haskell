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
    | highestValid values1 >= highestValid values2 = GT
    | otherwise = LT
    where 
        values1 = valueOfHand hand1
        values2 = valueOfHand hand2
        highestValid :: [Int] -> Int
        highestValid = (\xs -> if not $ null xs then maximum xs else -1) . filter (<= 21)


newDeck :: Deck
newDeck = [Card suit val | suit <- [Spades ..], val <- [Ace ..]]

printHand :: Hand -> IO()
printHand hand = putStrLn $ (++ ("\nWith possible values of " ++ intercalate ", " (map show $ valueOfHand hand))) $ intercalate ", " $ map show hand


valueOfHand :: Hand -> [Int]
valueOfHand hand = map sum $ mapM id listOfCardValues
    where listOfCardValues = map valueOfCard hand

valueOfCard :: Card -> [Int]
valueOfCard Joker          = [0]
valueOfCard (Card _ Ace)   = [1, 11]
valueOfCard (Card _ n)     = [k] 
    where k = min (1 + fromEnum n) 10

drawCard :: Deck -> (Card, Deck)
drawCard [] = error "Game over, Somehow your deck is out of cards."
drawCard (c:cs) = (c, cs)

shuffleDeck :: MonadRandom m => Deck -> m Deck
shuffleDeck deck = runRVar (shuffle deck) StdRandom
