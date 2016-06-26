
import Cards
-- A game state holds information regarding the current state of the game.
-- The first value is the game deck [Card]
-- The second value is the Users hand [Card]
-- The final value is the dealers hand [Card]
type GameState = (Deck, Deck, Deck)

main = do
    let deck = newDeck
    playGame (deck, [], [])

playGame :: GameState -> IO ()
playGame (deck, uHand, dHand)
    | uHand == [] = do
        putStrLn "You draw:"
        let 
            (fCard, newDeck) = drawCard deck
            (sCard, newDeck') = drawCard newDeck
            newUHand = fCard : sCard: []
            ()
        putStrLn $ show fCard
        putStrLn $ show sCard

    | otherwise = do
        putStrLn "In your hand you have: "
        printHand uHand

    --playGame (newDeck, newUHand, newDHand)
