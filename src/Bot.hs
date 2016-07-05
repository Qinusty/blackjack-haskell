{-# LANGUAGE UnicodeSyntax,RecordWildCards #-}

import Network
import System.IO
import Text.Printf
import Data.Char
import Data.List (intercalate)
import Control.Lens
import Data.Maybe
import qualified Data.Map.Strict as M
import Utils
import Cards
import FloodGate as FG

import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan
-- | 
data PlayerInfo = PlayerInfo {balance :: Int, pHand :: Hand
                             ,dHand :: Hand, playerState :: PlayerState} deriving (Eq)
data PlayerState = PLAYING Int | WAITING deriving (Eq)
data IRCMessage = Message {sender::String, typ::String,
                           target::String, message::String
                          } | PING String

instance Show IRCMessage where
    show Message { .. } = 
           "Sender: "  ++ sender ++ "\r\n" ++
           "typ: "     ++ typ    ++ "\r\n" ++
           "target: "  ++ target ++ "\r\n" ++
           "message: " ++ message ++ "\r\n"
    show _ = "Probably a ping, rip patterns"



data MPGameState = MPGameState {deck :: Deck, playerMap :: (M.Map String PlayerInfo)}

server :: String
server = "chat.freenode.net"
port :: Int
port = 6667
nick :: String
nick = "blackjack-hs-bot"

welcomeMsg = ["Welcome to my Blackjack IRC bot written in Haskell."
             ,"You can find the source code here https://github.com/Qinusty/blackjack-haskell/"
             ,"Feel free to open issues or make pull requests."]

commands :: [(String, String)]
commands = [
            ("help",     "help: This prints the help menu to the channel.")
            ,("bet",     "bet <Value>: This places a bet for the upcoming hand.")
            ,("hit",     "hit: This gets another card drawn from the deck into your hand.") 
            ,("stay",    "stay: This tells the dealer you're sticking with your current hand.")
            ,("balance", "balance: This requests your balance from the bot.")
            ,("hand",    "hand: This requests your current hand and value be shown.")
            ]

helpList :: [String]
helpList = [snd c | c <- commands]

withinBetBounds :: Int -> Bool
withinBetBounds x = x >= minBet && x <= maxBet

minBet = 10
maxBet = 1000

startingBalance :: Int
startingBalance = 5000

currency :: Char
currency = '£'

main :: IO()
main = do
    h    <- connectTo server (PortNumber (fromIntegral port))
    chan <- atomically $ newTChan
    hSetBuffering h NoBuffering
    write h "NICK" nick
    write h "USER" (nick++" 0 * :Haskell IRC Bot")
    --write h "JOIN" chan
    deck <- shuffleDeck newDeck
    forkIO $ FG.runThread chan
    listen h chan (MPGameState deck (M.empty)) -- Init the MPGameState


write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf    "> %s %s\n" s t

listen :: Handle -> TChan (IO ()) -> MPGameState -> IO ()
listen h chan state@(MPGameState deck ps)
    | length deck < 26 = do 
        aNewDeck <- shuffleDeck newDeck
        listen h chan (MPGameState (deck ++ aNewDeck) ps) 
    | otherwise = do
        s <- hGetLine h
        let parsedMessage = parseMessage s
        --print parsedMessage -- debug
        newState <- handleMessage h chan state $ parseMessage s
        
        listen h chan newState

newPlayer :: String -> PlayerInfo
newPlayer uName = PlayerInfo startingBalance [] [] WAITING

handleMessage :: Handle -> TChan (IO ()) -> MPGameState -> IRCMessage -> IO MPGameState 
handleMessage h _ state (PING serv) = do -- responds to a ping with PONG
            write h "PONG" serv
            return state
handleMessage h chan state msg@(Message sender typ target message) = 
            if typ == "PRIVMSG" && userNick /= nick then
                handleCommand h chan state (Message userNick typ target message)
            else
                return state
    where userNick = parseUserNick sender

handleCommand :: Handle -> TChan (IO ()) -> MPGameState -> IRCMessage -> IO MPGameState
handleCommand h chan state@(MPGameState deck ps) msg@(Message sender typ target message) = do 
    if player /= Nothing then
        case command of
            ":help" -> do 
                        mapM_ (sendMsg h chan sender) helpList
                        return state
            ":bet"  -> do 
                        case playerState (fromJust player) of
                            WAITING -> do 
                                let val        = (read (head args) :: Int)
                                    justPlayer = fromJust player                              
                                    (newPCards, deck') = drawCards 2 deck
                                    (newDCard, deck'') = drawCard deck'
                                    player'            = addToDealerHand [newDCard] $ addToPlayerHand newPCards $ makeBet val justPlayer
                                    ps'                = M.insert sender player' ps
                                if args /= [] then
                                    if withinBetBounds val then do
                                        if val <= (balance justPlayer) then do
                                            sendMsg h chan sender ("You're in with a bet of " ++ show val)
                                            sendMsg h chan sender $ "You draw " ++ showHand newPCards
                                            sendMsg h chan sender $ "Dealer draws " ++ showHand [newDCard]
                                            sendMsg h chan sender   "Use commands 'hit' or 'stay' to hit or stay respectively"
                                            return (MPGameState deck'' ps')
                                        else do
                                            sendMsg h chan sender $ "You cannot afford this bet! Your balance is " ++ show (balance justPlayer)
                                            return state
                                    else do
                                        sendMsg h chan sender  $ "This is not within the betting bounds! (" ++ show minBet ++ "-" ++ show maxBet ++ ")." 
                                        return state
                                else do 
                                    sendMsg h chan sender "You need to provide a value for your bet. bet <Value>"
                                    return state
                            otherwise -> do
                                sendMsg h chan sender "You're already playing a hand, use the 'hit' command to get another card."
                                return state
            ":hit"  -> do
                        case playerState (fromJust player) of
                            PLAYING _ -> do
                                let (card',deck') = drawCard deck
                                    justPlayer    = fromJust player
                                    player'       = addToPlayerHand [card'] justPlayer
                                    minHandVal    = minimum $ valueOfHand (pHand player')
                                    bust          = minHandVal > 21
                                    ps'           = if bust then
                                                        M.insert sender (endHand player') ps
                                                    else 
                                                        M.insert sender player' ps

                                sendMsg h chan sender $ "You draw: " ++ show card'
                                if bust then do
                                    sendMsg h chan sender $ "Your new minimum hand value is " ++ show minHandVal
                                    sendMsg h chan sender   "You have gone bust! You are now removed from this hand."
                                    sendMsg h chan sender   "To join back use the 'bet <Value>' command!"
                                else do
                                    sendMsg h chan sender $ "Your new hand is: " ++ showHand (pHand player')
                                    sendMsg h chan sender   "Use commands 'hit' or 'stay' to hit or stay respectively"
                                return (MPGameState deck' ps')
                            otherwise -> do 
                                sendMsg h chan sender "You can't hit because you haven't placed a bet for this hand! Use command 'bet <Value> to place a bet."
                                return state
            ":balance" -> do 
                        sendMsg h chan sender $ "Your balance is: " ++ (currency : show (balance (fromJust player)))
                        return state
            ":hand" -> case playerState (fromJust player) of
                            PLAYING _ -> do
                                sendMsg h chan sender $ "Your current hand is: " ++ showHand (pHand (fromJust player))
                                return state
                            otherwise -> do
                                sendMsg h chan sender "You're not currently playing a hand, use command 'bet <Value>' to begin."
                                return state
            ":stay" -> case playerState (fromJust player) of
                            PLAYING bet -> do
                                let justPlayer = fromJust player
                                    playerHand = pHand justPlayer
                                    (dealerHand,deck') = dealerDraws (dHand justPlayer) deck playerHand
                                    won = compareHand playerHand dealerHand == GT
                                    player' = if won then 
                                                endHand $ modifyBalance ((balance justPlayer) + bet*2) justPlayer
                                              else
                                                endHand justPlayer
                                    ps' = M.insert sender player' ps

                                sendMsg h chan sender   "Final Hands:"
                                sendMsg h chan sender $ "Your hand: "   ++ showHand playerHand
                                sendMsg h chan sender $ "Dealer hand: " ++ showHand dealerHand
                                if won then do
                                    sendMsg h chan sender $ "You won! You receive " ++ show (bet*2) ++ 
                                                        " and your new balance is " ++ (currency : show (balance (player')))
                                else do
                                    sendMsg h chan sender "You lose!"
                                    sendMsg h chan sender "To play another hand use the 'bet <Value>' command!"
                                return (MPGameState deck' ps')

                            otherwise -> do
                                sendMsg h chan sender "You're not currently playing a hand, use command 'bet <Value>' to begin."
                                return state
            ":players" -> do 
                    sendMsg h chan sender $ "List of players: " ++ (intercalate ", " $ M.keys ps)
                    return state
            otherwise -> do
                sendMsg h chan sender "That isn't a valid command! Use the 'help' command to see valid commands."
                return state

    else do
        let newPs = M.insert sender (newPlayer sender) ps
        -- Sorry for cheap hack
        mapM_ (sendMsg h chan sender) welcomeMsg
        sendMsg h chan sender "You have been added to the player list, feel free to check available commands by using the 'help' command"
        return (MPGameState deck newPs)

    where messageWords = words $ map (toLower) message
          command = head messageWords
          args = tail messageWords
          player = M.lookup sender ps

makeBet :: Int -> PlayerInfo -> PlayerInfo
makeBet betVal PlayerInfo { .. } = PlayerInfo (balance - betVal) pHand dHand (PLAYING betVal)

modifyBalance :: Int -> PlayerInfo -> PlayerInfo
modifyBalance newBalance PlayerInfo { .. } = PlayerInfo newBalance pHand dHand playerState

endHand :: PlayerInfo -> PlayerInfo 
endHand PlayerInfo { .. } = PlayerInfo balance [] [] WAITING 

addToPlayerHand :: Hand -> PlayerInfo -> PlayerInfo
addToPlayerHand h PlayerInfo { .. } = PlayerInfo balance newHand dHand playerState
    where newHand = pHand ++ h 

addToDealerHand :: Hand -> PlayerInfo -> PlayerInfo
addToDealerHand h PlayerInfo { .. } = PlayerInfo balance pHand (dHand ++ h) playerState

-- | Takes the dealers hand, the deck and the players hand.
dealerDraws :: Hand -> Deck -> Hand -> (Hand, Deck)
dealerDraws dHand deck@(c:cs) pHand
    | compareHand dHand pHand == GT = (dHand, deck) -- if already better
    | (minimum $ valueOfHand dHand) < 17   = let (dHand',deck') = dealerDraws (c:dHand) cs pHand in (dHand', deck')
    | otherwise                          = (dHand,deck) -- 17 or more / BUST

-- | Returns a tuple (sender, type, target, message)
parseMessage :: String -> IRCMessage
parseMessage s = if startsWith "PING :" s then
                    let serv = tail $ (!! 1) $ words s
                    in PING serv
                 else
                    let parts     = words s
                        sender    = head parts
                        typ       = parts !! 1
                        target    = parts !! 2
                        message   = unwords $ drop 3 parts
                    in Message sender typ target message

sendMsg :: Handle -> TChan (IO ()) -> String -> String -> IO ()
sendMsg h chan target msg = atomically $ writeTChan chan $ write h "PRIVMSG" (target ++ (' ' : ':' : msg) ++ "\r\n")

parseUserNick :: String -> String
parseUserNick s = takeWhile (\c -> c /= '!') $ tail s


