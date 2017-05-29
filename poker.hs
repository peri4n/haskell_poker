-- Model of Texas Holdem

import Data.Foldable as F
import Data.Map(Map, fromListWith)
import Data.List(map)
import Data.Function

data Suit = Diamonds | Hearts | Spades | Clubs deriving (Enum, Ord, Eq)

instance Show Suit where
    show Clubs = "♣ "
    show Spades = "♠ " 
    show Hearts = "♥ "
    show Diamonds = "♦ "

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Enum, Ord, Eq)

instance Show Value where
    show Ace = "A"
    show King = "K"
    show Queen = "Q"
    show Jack = "J"
    show Ten = "T"
    show Nine = "9"
    show Eight = "8"
    show Seven = "7"
    show Six = "6"
    show Five = "5"
    show Four = "4"
    show Three = "3"
    show Two = "2"

data Card = Card {
    suit :: Suit, 
    value :: Value } deriving (Eq)

toPair :: Card -> (Suit, Value)
toPair x = (suit x, value x)

instance Ord Card where
    compare = compare `on` value

instance Show Card where
    show (Card suit value) = (show suit) ++ (show value)

type Cards = [Card]

deck :: Cards
deck = [ Card s v | s <- [Diamonds .. Clubs], v <- [Two .. Ace]]

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

-- All values should be present in sorted order
data Rank = HighCard [Value] | -- All 5 cards
    Pair Value Cards | -- The pair and 3 kickers
    TwoPair Value Value Card | -- Two pairs a kicker
    ThreeOfAKind Value Cards | -- The tripple and two kickers
    Straight Value | -- High card of the straigt
    Flush Value | -- High card of the flush
    FullHouse  Value Value | -- Tripples and pair
    FourOfAKind Value Value | -- Quadruple and a kicker
    StraightFlush Value -- High card of straight and flush
    deriving (Eq)

instance Show Rank where
    show (HighCard xs) = (show $ maximum xs) ++ "high with kickers: " ++ (show xs)
    show (Pair p xs) = "Paif of " ++ (show p) ++ "s with kickers: " ++ (show xs)
    show (TwoPair p1 p2 x) = "Pair of " ++ (show p1) ++ " and " ++ (show p2) ++ "with kicker " ++ (show x)

instance Ord Rank where

    compare (HighCard xs) (HighCard ys) = compare xs ys
    compare (HighCard _ ) _ = LT
    compare _ (HighCard _ ) = GT



