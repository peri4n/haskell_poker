module Poker.Cards
  ( Value
  , Suit
  , Card
  , Cards
  , suit
  , value
  , values
  , newDeck
  , groupByValue
  , groupBySuit
  ) where

import           Data.Function (on)
import           Data.Map      (Map, fromListWith)
import           System.Random

data Suit
  = Diamonds
  | Hearts
  | Spades
  | Clubs
  deriving (Enum, Ord, Eq)

instance Show Suit where
  show Clubs    = "♣ "
  show Spades   = "♠ "
  show Hearts   = "♥ "
  show Diamonds = "♦ "

data Value
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Enum, Ord, Eq)

instance Show Value where
  show Ace   = "A"
  show King  = "K"
  show Queen = "Q"
  show Jack  = "J"
  show Ten   = "T"
  show Nine  = "9"
  show Eight = "8"
  show Seven = "7"
  show Six   = "6"
  show Five  = "5"
  show Four  = "4"
  show Three = "3"
  show Two   = "2"

data Card = Card
  { suit  :: Suit
  , value :: Value
  } deriving (Eq)

type Cards = [Card]

values :: Cards -> [Value]
values = map value

instance Ord Card where
  compare = compare `on` value

instance Show Card where
  show (Card suit value) = show suit ++ show value

newDeck :: Cards
newDeck = [Card s v | s <- [Diamonds .. Clubs], v <- [Two .. Ace]]

groupBySuit :: Cards -> Map Suit Cards
groupBySuit cs = fromListWith (++) $ map withSuit cs
  where
    withSuit :: Card -> (Suit, [Card])
    withSuit c = (suit c, [c])

groupByValue :: Cards -> Map Value Cards
groupByValue cs = fromListWith (++) $ map withValue cs
  where
    withValue :: Card -> (Value, [Card])
    withValue c = (value c, [c])
