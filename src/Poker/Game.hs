
module Poker.Game where

import Data.Function(on)
import Data.List(map, sortBy)

import Poker.Cards (Cards, Card, shuffledDeck)
import Poker.Ranking

data Player = Player { name :: String
                     , cards :: Cards
                     } deriving Show

newPlayer :: String -> Player
newPlayer name = Player name []

dealCard :: Card -> Player -> Player
dealCard c (Player name cs) = Player name (c:cs)

data Game = Game [Player] Cards deriving Show

newGame :: Int -> IO Game
newGame n = Game (map (newPlayer . show) [1..n]) <$> shuffledDeck

dealCards' :: Game -> Game
dealCards' (Game players cards) = Game dealtPlayers restCards
    where
        (draws,restCards) = splitAt (length players) cards
        dealtPlayers = zipWith dealCard draws players

dealCards :: Int -> Game -> Game
dealCards n game = iterate dealCards' game !! n



