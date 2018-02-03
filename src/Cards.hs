-- Model of Texas Holdem

module Cards where

import Data.Function(on)

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

type Cards = [Card]

instance Ord Card where
    compare = compare `on` value

instance Show Card where
    show (Card suit value) = (show suit) ++ (show value)

