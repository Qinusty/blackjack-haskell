
{-# LANGUAGE UnicodeSyntax #-}
import Cards

-- A game state holds information regarding the current state of the game.
-- The first value is the game deck [Card]
-- The second value is the Users hand [Card]
-- The third value is the dealers hand [Card]
-- The final value represents USER BUST?
type GameState = (Deck, Deck, Deck, Bool)

main :: IO ()
main = do
    deck <- shuffleDeck newDeck
    playGame (deck, [], [], False)

playGame :: GameState -> IO ()
playGame state@(deck, uHand, dHand, _)
    | null uHand && null dHand = do -- INIT CASE
        putStrLn "You draw:"
        let (fCard, nDeck) = drawCard deck
            (sCard, nDeck') = drawCard nDeck
            newUHand = [fCard, sCard]
            (dCard, nDeck'') = drawCard nDeck'
            newDHand = [dCard]
        print fCard
        print sCard
        -- dealer            
        putStrLn "\nThe dealer drew: "
        print dCard

        playGame (nDeck'', newUHand, newDHand, False)

    | otherwise = do -- Main Game
        newState  <- userTurn state
        endState <- dealerTurn newState
        endGame endState

endGame :: GameState -> IO ()
endGame (_, uHand, dHand, _) = do
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
userTurn state@(deck, uHand, dHand, _) = do 
    putStrLn "\nYour current hand is:"
    printHand uHand
    putStrLn "\nWould you like to hit, stay? (enter h or s)"
    choice <- getLine
    performAction choice
    where
        performAction [] = do
            putStrLn "Invalid choice"
            userTurn state
        performAction (c:_)
            | c == 'h' = do
                let (card, nDeck) = drawCard deck
                    newUHand = card : uHand
                putStr "\nYou drew: "
                print card
                if minimum  (valueOfHand newUHand) <= 21 then
                    userTurn (nDeck, newUHand, dHand, False)
                else do
                    putStrLn "BUST!"
                    return (nDeck, newUHand, dHand, True)
            | c == 's' = return state
            | otherwise     = do 
                putStrLn "Invalid choice! Try again"
                userTurn state

dealerTurn :: GameState -> IO GameState
dealerTurn state@(deck, uHand, dHand, userBust) 
    | userBust                          = return state
    | minimum (valueOfHand dHand) <= 16 = do
        let (card, nDeck) = drawCard deck
            newDHand = card : dHand
        putStr "\nDealer draws: "
        print card
        dealerTurn (nDeck, uHand, newDHand, False)
    | otherwise = return state
