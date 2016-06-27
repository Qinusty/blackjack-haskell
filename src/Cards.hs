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
        highestValid (a,b)
            | (a >= b) && a <= 21 = a
            | (b >= a) && b <= 21 = b
            | otherwise           = -1


newDeck :: Deck
newDeck = [Card suit val | suit <- [Spades ..], val <- [Ace ..]]

printHand :: Hand -> IO()
printHand hand = putStrLn $ (++ ("\nWith a value of " ++ show (valueOfHand hand))) $ intercalate ", " $ map show hand

-- in case of two aces, user is stuck with 1 or 11 for both. :/
valueOfHand :: Hand -> (Int, Int)
valueOfHand = foldr ((\(a,b) (c,d) -> (a+c, b+d)) . valueOfCard) (0,0)

-- NEEDS SIMPLIFYING 
valueOfCard :: Card -> (Int, Int)
valueOfCard (Card _ Ace)   = (1, 11)
valueOfCard (Card _ Two)   = (2, 2)
valueOfCard (Card _ Three) = (3, 3)
valueOfCard (Card _ Four)  = (4, 4)
valueOfCard (Card _ Five)  = (5, 5)
valueOfCard (Card _ Six)   = (6, 6)
valueOfCard (Card _ Seven) = (7, 7)
valueOfCard (Card _ Eight) = (8, 8)
valueOfCard (Card _ Nine)  = (9, 9)
valueOfCard (Card _ Ten)   = (10,10)
valueOfCard (Card _ Jack)  = (10,10)
valueOfCard (Card _ Queen) = (10,10)
valueOfCard (Card _ King)  = (10,10)
valueOfCard Joker          = (0,0)

drawCard :: Deck -> (Card, Deck)
drawCard [] = error "Empty Deck"
drawCard (c:cs) = (c, cs)

shuffleDeck :: MonadRandom m => Deck -> m Deck
shuffleDeck deck = runRVar (shuffle deck) StdRandom
