module Interface where

import Data.Char
import Setup
import System.Random
import Data.Maybe(fromJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO.Unsafe




-- main function

runGame :: IO ()
runGame = do
  putStrLn "Welcome to the game."
  gameLoop (massShuffle fullDeck  10 (mkStdGen (unsafePerformIO (getStdRandom (randomR(1,9223372036854775807)))))) Empty Empty "Player 1" False False

--Defines the turn of a player (if he chooses to draw a card or not)

turn :: Hand -> Hand -> String -> (Hand,Hand, Bool)
turn deck hand yn = if null yn || not (map toLower yn == "n") then case (draw deck hand) of
                                                                     (x, y) -> (x, y, False)
                 else (deck,hand, True)

-- Game loop of the server. It ends whenever both players either go bust or type "n" to end their turn                

gameLoop :: Hand -> Hand -> Hand -> String -> Bool -> Bool -> IO ()
gameLoop deck playerHand1 playerHand2 currentPlayer over1 over2 = do
  if currentPlayer == "Player 1" then do
   putStrLn "-----------------------------------------------------------------\n"
   putStrLn "                       Player 1's turn\n"
   putStrLn $ currentPlayer ++ "'s score: " ++ show (value playerHand1)
   putStrLn $ currentPlayer ++ "'s hand: " ++ display playerHand1 ++ "\n"
  else do
   putStrLn "-----------------------------------------------------------------\n"
   putStrLn "                       Player 2's turn\n"
   putStrLn $ currentPlayer ++ "'s score: " ++ show (value playerHand2) 
   putStrLn $ currentPlayer ++ "'s hand: " ++ display playerHand2 ++ "\n"
  if ((gameOver playerHand1) || over1) && ((gameOver playerHand2) || over2) then do
    finish deck playerHand1 playerHand2
  else do 
   putStrLn (   "Draw a card? [y/n] ")
   yn <- getLine
   if currentPlayer == "Player 1" then do
     if ((gameOver playerHand1) || over1) then do
      gameLoop deck playerHand1 playerHand2 "Player 2" over1 over2
     else do 
       let (deck',hand', isOver)=turn deck playerHand1 yn
       gameLoop deck' hand' playerHand2 "Player 2" isOver over2
   else do 
    if ((gameOver playerHand2) || over2) then do
     gameLoop deck playerHand1 playerHand2 "Player 1" over1 over2
    else do 
      case turn deck playerHand2 yn of
        (deck',hand', isOver) -> gameLoop deck' playerHand1 hand' "Player 1" over1 isOver

--Display the final scores and the winners

finish :: Hand -> Hand -> Hand -> IO ()
finish deck playerHand1 playerHand2 = do
  putStrLn "-----------------------------------------------------------------\n"
  putStrLn $ "Player1's final score: " ++ show( value playerHand1) 
  putStrLn $ "Player1's final hand: " ++ display playerHand1 ++ "\n"
  putStrLn $ "Player2's final score: " ++ show( value playerHand2) 
  putStrLn $ "Player2's final hand: " ++ display playerHand2 ++ "\n"
  putStrLn $ "The bank's final score: "++ show (value bank) 
  putStrLn $ "Bank's final hand: " ++ display bank ++ "\n"
  putStrLn $ "Winner: " ++ show (winner playerHand1 playerHand2 bank)
  where
  bank = bankDraw deck

