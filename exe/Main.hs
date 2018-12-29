module Main where

import Poker.Cards
import Poker.Game

main :: IO ()
main = putStrLn $ concat $ map show deck
