module Ranking where

import Cards

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
