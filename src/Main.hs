module Main where

import Cards

main :: IO ()
main = putStrLn $ concat $ map show deck
