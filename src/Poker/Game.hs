module Poker.Game where

import           Control.Monad.State
import           Data.Function         (on)
import           Data.List             (map, sortBy)
import           System.Random
import           System.Random.Shuffle

import           Poker.Cards           (Card, Cards, newDeck)
import           Poker.Ranking

type Draw = State Game

newtype Game = Game { deck    :: Cards } deriving (Show)

draw :: Draw Card
draw = state $ \(Game (c:cs)) -> (c, Game cs)

draws :: Int -> Draw Cards
draws n = replicateM n draw

newGame :: IO Game
newGame = do
  Game . shuffledDeck <$> getStdGen


shuffledDeck :: StdGen -> Cards
shuffledDeck = shuffle' newDeck 52
