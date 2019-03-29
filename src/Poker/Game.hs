module Poker.Game where

import           Data.Function (on)
import           Data.List     (map, sortBy)
import           System.Random

import           Poker.Cards   (Card, Cards, shuffledDeck)
import           Poker.Ranking

data Player = Player
  { name  :: String
  , cards :: Cards
  } deriving (Show)

newPlayer :: String -> Player
newPlayer name = Player name []

dealCard :: Card -> Player -> Player
dealCard c (Player name cs) = Player name (c : cs)

data Game =
  Game [Player]
       Cards
       StdGen
  deriving (Show)

newGame :: Config -> Game
newGame config = Game (map (newPlayer . show) [1 .. (players config)]) deck rng
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

data Config = Config
  { players :: Int
  , seed    :: Int
  }
