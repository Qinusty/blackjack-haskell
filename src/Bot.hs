{-# LANGUAGE UnicodeSyntax #-}

import Network
import System.IO
import Text.Printf
import Utils
--import Cards
-- | Nick
type Player = (String, Int)
data IRCMessage = Message String String String String | PING String

server :: String
server = "chat.freenode.net"
port :: Int
port = 6667
chan :: String
chan = "#blackjack-hs"
nick :: String
nick = "blackjack-hs-bot"

commands :: [String]
commands = [
            "help"
            ,"bet"
            ,"hit"
            ,"stay"
            ,"balance"
            ,"hand"
            ]

betBounds :: [Int]
betBounds = [10, 1000]

startingBalance :: Int
startingBalance = 5000

main :: IO()
main = do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    write h "NICK" nick
    write h "USER" (nick++" 0 * :Haskell IRC Bot")
    write h "JOIN" chan
    listen h


write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf    "> %s %s\n" s t

listen :: Handle -> IO ()
listen h = forever $ do
    s <- hGetLine h
    if startsWith "Ping :" s then
        write h "Pong" server
    else
        handleMessage h $ parseMessage s
  where
    forever a = do a; forever a

handleMessage :: Handle -> IRCMessage -> IO() 
handleMessage h (PING serv)                       = write h "PING" serv
handleMessage h (Message sender typ target message) = 
            if typ == "PRIVMSG" then
                if startsWith nick $ tail message then
                    sendMsg h target ((parseUserNick sender) ++ ": BOOP!")
                else return ()
            else
                    return ()

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

sendMsg :: Handle -> String -> String -> IO ()
sendMsg h target msg = write h "PRIVMSG" ((target ++ (' ' : ':' : msg)) ++ "\r\n")

parseUserNick :: String -> String
parseUserNick s = takeWhile (\c -> c /= '!') $ tail s


