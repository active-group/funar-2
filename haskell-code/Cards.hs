module Cards where

-- Modell für das franz. Blatt

-- Eine Spielkarte besteht aus:
-- - Farbe (Suit)
-- - Wert (Rank)

data Suit = Clubs | Spades | Hearts | Diamonds
    deriving (Eq, Show)

data Rank =
    One | Two | Three | Four | Five | Six | Seven
    | Eight | Nine | Ten | Jack | Queen | King
    | Ace
    deriving (Eq, Show)

data Card = Card { suit :: Suit, rank :: Rank }
    deriving (Eq, Show)