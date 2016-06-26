
{-# LANGUAGE UnicodeSyntax #-}
import Cards

-- A game state holds information regarding the current state of the game.
-- The first value is the game deck [Card]
-- The second value is the Users hand [Card]
-- The final value is the dealers hand [Card]
type GameState = (Deck, Deck, Deck)

main :: IO ()
main = do
    deck <- shuffleDeck newDeck
    playGame (deck, [], [])

playGame :: GameState -> IO ()
playGame state@(deck, uHand, dHand)
    | null uHand && null dHand = do -- INIT CASE
        putStrLn "You draw:"
        let 
            (fCard, nDeck) = drawCard deck
            (sCard, nDeck') = drawCard nDeck
            newUHand = [fCard, sCard]
            (dCard, nDeck'') = drawCard nDeck'
            newDHand = [dCard]
        print fCard
        print sCard
        -- dealer            
        putStrLn "\nThe dealer drew: "
        print dCard

        playGame (nDeck'', newUHand, newDHand)

    | otherwise = do -- Main Game
        newState  <- userTurn state
        state <- dealerTurn newState
        endGame state

endGame :: GameState -> IO ()
endGame (_, uHand, dHand) = do
    putStr "\nYour hand: "
    printHand uHand
    putStr "\nDealer hand: "
    printHand dHand
    putStrLn ("\n" ++ winnerText)
    where
        winnerText
            | compareHand uHand dHand == GT = "You win! Congratulations!"
            | otherwise                     = "You lose, Better luck next time :("

userTurn :: GameState -> IO GameState
userTurn state@(deck, uHand, dHand) = do 
    putStrLn "\nYour current hand is:"
    printHand uHand
    putStrLn "\nWould you like to hit, stay? (enter h or s)"
    choice <- getChar
    performAction choice
    where
        performAction choice
            | choice == 'h' = do
                let 
                    (card, nDeck) = drawCard deck
                    newUHand = card : uHand
                putStr "\nYou drew: "
                print card
                userTurn (nDeck, newUHand, dHand)
            | choice == 's' = return state
            | otherwise     = do 
                putStrLn "Invalid choice! Try again"
                userTurn state

dealerTurn :: GameState -> IO GameState
dealerTurn state@(deck, uHand, dHand) 
    | fst (valueOfHand dHand) <= 16 || snd (valueOfHand dHand) <= 16 = do
        let 
            (card, nDeck) = drawCard deck
            newDHand = card : dHand
        putStr "\nDealer draws: "
        print card
        dealerTurn (nDeck, uHand, newDHand)
    | otherwise = return state