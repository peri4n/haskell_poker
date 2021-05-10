module Main where

import           Poker.Cards
import           Poker.Game
import System.Random
import Control.Monad.State.Lazy
import Control.Parallel.Strategies

main :: IO ()
main = do
  gen <- getStdGen
  let threads = 1000
      seeds = take threads $ randoms gen
  let ds = parMap rpar drawHand seeds
  forM_ ds $ \d -> putStrLn (show d)

