module Interface where

import Data.Char
import Setup
import System.Random
import Data.Maybe(fromJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO.Unsafe


data Interface = Interface
  { iFullDeck :: Hand
  , iValue    :: Hand -> Int
  , iDisplay  :: Hand -> String
  , iGameOver :: Hand -> Bool
  , iWinner   :: Hand -> Hand -> Player
  , iDraw     :: Hand -> Hand -> (Hand, Hand)
  , ibankDraw :: Hand -> Hand
  , iShuffle  :: StdGen -> Hand -> (Hand,StdGen)
  , iMassShuffle :: Hand -> Int -> StdGen -> Hand
  }

implementation = Interface
  {  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iDisplay   = display
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  ibankDraw  = bankDraw
  ,  iShuffle   = shuffle
  ,  iMassShuffle   = massShuffle
  }

main :: IO ()
main = runGame implementation


-- Runs a game given an implementation of the interface.

runGame :: Interface -> IO ()
runGame i = do
  putStrLn "Welcome to the game."
  
  gameLoop i (iMassShuffle i (iFullDeck i) 10 (mkStdGen 10)) Empty

-- Play until the guest player is bust or chooses to stop.

gameLoop :: Interface -> Hand -> Hand -> IO ()
gameLoop i deck playerHand = do
  putStrLn $ "Your current score: " ++ show (iValue i playerHand) ++ "\n"
  putStrLn $ "Your current hand: " ++ iDisplay i playerHand ++ "\n"
  if iGameOver i playerHand then do
    finish i deck playerHand
   else do
    putStr (   "Draw "
            ++ (if playerHand==Empty then "a " else "another ")
            ++ "card? [y/n] ")
    yn <- getLine
    if null yn || not (map toLower yn == "n") then do
      let (deck', playerHand') = iDraw i deck playerHand
      gameLoop i deck' playerHand'
     else
      finish i deck playerHand

-- Display the bank's final score and the winner.

finish :: Interface -> Hand -> Hand -> IO ()
finish i deck playerHand = do
  putStrLn $ "Your final score: " ++ show( iValue i playerHand)
  putStrLn $ "Your final hand: " ++ iDisplay i playerHand
  putStrLn $ "The bank's final score: "++ show (iValue i bank)
  putStrLn $ "Bank's final hand: " ++ iDisplay i bank
  putStrLn $ "Winner: " ++ show (iWinner i playerHand bank)
  where
  bank = ibankDraw i deck

