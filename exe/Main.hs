module Main where

import           Poker.Cards
import           Poker.Game
import Control.Monad.State.Lazy

main :: IO ()
main = do
  game <- newGame
  let ds = evalState (draws 5) game
  print ds

