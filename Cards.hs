module Cards where
    import Data.List (intercalate)
    data Suit = Spades | Clubs | Diamonds | Hearts deriving (Show, Enum, Eq)
    data Value = Ace | Two | Three | Four | Five | Six | Seven
                | Eight | Nine | Ten | Jack | Queen | King 
                deriving (Show, Enum, Eq)

    data Card = Card Suit Value | Joker deriving (Eq)
    type Deck = [Card]

    instance Show Card where
        show (Card suit val) = show val ++ " Of " ++ show suit

    newDeck = [Card suit val | suit <- [Spades ..], val <- [Ace ..]]

    printHand :: Deck -> IO()
    printHand = putStrLn . intercalate ", " . map show

    drawCard :: Deck -> (Card, Deck)
    drawCard [] = error "Empty Deck"
    drawCard (c:cs) = (c, cs)


    