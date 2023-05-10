module Cards where

-- Modell für das franz. Blatt

-- Eine Spielkarte besteht aus:
-- - Farbe (Suit)
-- - Wert (Rank)

data Suit = Clubs | Spades | Hearts | Diamonds
    deriving (Eq, Show, Enum, Bounded)

allSuits :: [Suit]
allSuits = [minBound .. maxBound]
-- >>> allSuits
-- [Clubs,Spades,Hearts,Diamonds]

data Rank =
    One | Two | Three | Four | Five | Six | Seven
    | Eight | Nine | Ten | Jack | Queen | King
    | Ace
    deriving (Eq, Show)

data Card = Card { suit :: Suit, rank :: Rank }
    deriving (Eq, Show)

-- Liste aller Karten
allCards :: [Card]
allCards = []
