module Cards where

-- Modell für das franz. Blatt

-- Eine Spielkarte besteht aus:
-- - Farbe (Suit)
-- - Wert (Rank)

data Suit = Clubs | Spades | Hearts | Diamonds
    deriving (Eq, Show)