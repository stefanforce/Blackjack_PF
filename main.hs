module Main where

import Data.Char
import Setup
import Interface
import System.Random
import Data.Maybe(fromJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO.Unsafe

main :: IO ()
main = runGame 