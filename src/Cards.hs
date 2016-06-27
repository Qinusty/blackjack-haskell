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

valueOfCard :: Card -> (Int, Int)
valueOfCard (Card _ Ace)   = (1, 11)
valueOfCard (Card _ Jack)  = (10,10)
valueOfCard (Card _ Queen) = (10,10)
valueOfCard (Card _ King)  = (10,10)
valueOfCard Joker          = (0,0)
valueOfCard (Card _ n)     = (k+1, k+1) where k = fromEnum n

drawCard :: Deck -> (Card, Deck)
drawCard [] = error "Game over, Somehow your deck is out of cards."
drawCard (c:cs) = Just (c, cs)

shuffleDeck :: MonadRandom m => Deck -> m Deck
shuffleDeck deck = runRVar (shuffle deck) StdRandom
