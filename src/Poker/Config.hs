module Poker.Config where

data Config = Config
  { nrOfPlayers :: Int
  , seed        :: Int
  } deriving (Show)
