# Blackjack Game
This is my basic blackjack game written in Haskell. It runs in a terminal window and allows the user to play against a dealer who follows a set of rules on whether to draw a card or fold. The user can decide whether or not to fold throughout the game.

## Running the game
Compile the Application with `ghc Game.hs --make`

Run the application with `./Game`

## The IRC Bot

### Messaging
The bot is currently running with username blackjack-hs-bot on freenode. PM to interact with the bot.

Your first message initiates contact with the bot and then further messages are taken as commands. 

**Bet** followed by a number will start a hand and allow you to hit/stay.

To draw another card, send the **hit** command.

Once you send the **stay** command, the dealer will then draw cards until his hand has a higher value than yours or he reaches a value of 17 or higher.

- If the player has an equal or higher value hand than the dealer, he wins.
- If the player goes bust or has a lower value hand than the dealer, he loses.
### Running the bot
Once you have all of the dependencies you can compile and run the Bot with
```
ghc Bot.hs
./Bot
```
**Ensure that you have given your bot an appropriate name within the Bot.hs source file**

## Dependencies
```
sudo dnf install Haskell-Platform
cabal install random-extras-0.19
```
