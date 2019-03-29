module Main where

import           Poker.Cards
import           Poker.Config
import           Poker.Game
import           System.Random

main :: IO ()
main = do
  seed <- randomIO
  putStrLn $ "Generating seed:" ++ show seed ++ ""
  let playerRng = mkStdGen seed
      (players, _) = randomR (2, 10) playerRng
      config = Config players seed
  putStrLn $ "Generating new game with configuration: " ++ show config
  let game = newGame config
  putStrLn "Dealing 2 cards two each player."
  print $ dealCards 2 game
