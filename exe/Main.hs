module Main where

import Poker.Cards
import Poker.Game
import System.Random

main :: IO ()
main = do
    players <- randomRIO (2, 10)
    putStrLn $ "Generating new game with " ++ show players ++ " players."
    game <- newGame players
    print $ dealCards 2 game
