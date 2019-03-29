module Poker.Game where

import           Control.Monad.State
import           Data.Function         (on)
import           Data.List             (map, sortBy)
import           System.Random
import           System.Random.Shuffle

import           Poker.Cards           (Card, Cards, newDeck)
import           Poker.Config
import           Poker.Ranking

data Player = Player
  { name  :: String
  , cards :: Cards
  } deriving (Show)

newPlayer :: String -> Player
newPlayer name = Player name []

dealCard :: Card -> Player -> Player
dealCard c (Player name cs) = Player name (c : cs)

type Step = State Game

data Game = Game
  { players :: [Player]
  , deck    :: Cards
  , rng     :: StdGen
  } deriving (Show)

newGame :: Config -> Game
newGame config =
  Game (map (newPlayer . show) [1 .. (nrOfPlayers config)]) deck rng
  where
    rng = mkStdGen (seed config)
    deck = shuffledDeck rng

dealCards' :: Game -> Game
dealCards' (Game players cards rng) = Game dealtPlayers restCards rng
  where
    (draws, restCards) = splitAt (length players) cards
    dealtPlayers = zipWith dealCard draws players

dealCards :: Int -> Game -> Game
dealCards n game = iterate dealCards' game !! n

shuffledDeck :: StdGen -> Cards
shuffledDeck = shuffle' newDeck 52
