
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
