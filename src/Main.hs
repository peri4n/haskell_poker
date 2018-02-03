module Main where

import Cards
import Game

main :: IO ()
main = putStrLn $ concat $ map show deck
