-- Model of Texas Holdem

import Data.SortedList

data Suit = Diamonds | Hearts | Spades | Clubs deriving (Enum, Ord, Eq)

instance Show Suit where
    show Clubs = "♣"
    show Spades = "♠" 
    show Hearts = "♥"
    show Diamonds = "♦"

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
    value :: Value } deriving (Eq, Ord)

instance Show Card where
    show (Card suit value) = (show suit) ++ " " ++ (show value)

type Cards = SortedList Card

suits :: [Suit]
suits = [Diamonds .. Clubs]

values :: [Value]
values = [Two .. Ace]

deck :: Cards
deck = toSortedList $ [ Card s v | s <- suits, v <- values]

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



