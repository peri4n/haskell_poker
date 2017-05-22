-- Model of Texas Holdem

data Suit = Clubs | Spades | Hearts | Diamonds deriving (Enum, Eq, Show)

data Value = Ace | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two deriving (Enum, Eq, Show)

data Card = Card Suit Value deriving (Eq, Show)

type Cards = [Card]

suits :: [Suit]
suits = [Clubs .. Diamonds]

values :: [Value]
values = [Ace .. Two]

deck :: Cards
deck = [ Card s v | s <- suits, v <- values]

data Rank = HighCard Value Value Value Value Value | -- All 5 cards
    Pair Value Value Value Value | -- The pair and 3 kickers
    TwoPair Value Value Value | -- Two pairs a kicker
    ThreeOfAKind Value Value Value | -- The tripple and two kickers
    Straight Value | -- High card of the straigt
    Flush Value | -- High card of the flush
    FullHouse  Value Value | -- Tripples and pair
    FourOfAKind Value Value | -- Quadruple and a kicker
    StraightFlush Value -- High card of straight and flush
