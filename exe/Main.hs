module Main where

import           Poker.Cards
import           Poker.Game
import           System.Random

main :: IO ()
main = do
  seed <- randomIO
  putStrLn $ "Generating seed:" ++ show seed ++ ""
  let playerRng = mkStdGen seed
  let (players, _) = randomR (2, 10) playerRng
  putStrLn $ "Generating new game with " ++ show players ++ " players."
  let game = newGame (Config players seed)
  putStrLn "Dealing 2 cards two each player."
  print $ dealCards 2 game
