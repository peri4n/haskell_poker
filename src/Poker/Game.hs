
module Poker.Game where

import Data.Foldable as F
import Data.Function(on)
import Data.Map(Map, fromListWith)
import Data.List(map, sortBy)
import System.Random

import Poker.Cards
import Poker.Ranking

keyFrom :: (a -> b) -> a -> (b, [a])
keyFrom f x = (f x, [x])

groupBy :: (Ord b) => (a -> b) -> [a] -> Map b [a]
groupBy f xs = fromListWith (++) (map (keyFrom f) xs)

groupBySuit :: Cards -> Map Suit Cards
groupBySuit = groupBy suit

groupByValue :: Cards -> Map Value Cards
groupByValue = groupBy value

values :: Cards -> [Value]
values = map value

makeHighCard :: Cards -> Maybe Rank
makeHighCard xs = if (F.null xs) then Nothing else Just (HighCard $ values xs)

augment :: (RandomGen g) => g -> Cards -> ([(Card, Int)], g)
augment gen [] = ([], gen)
augment gen (x:xs) = ((x, draw):rest, g)
    where
        (draw, newGen) = random gen
        (rest, g) = augment newGen xs

shuffle :: (RandomGen g) => g -> Cards -> (Cards, g)
shuffle g xs = ((map fst xxs), ng)
    where
        (axs, ng) = augment g xs
        xxs = sortBy (compare `on` snd) axs

