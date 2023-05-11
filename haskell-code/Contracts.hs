module Contracts where

-- Zero-Coupon-Bond, einfaches Beispiel für Vertrag
-- "Ich bekomme am 24.12.2023 100 Euro."

-- Currency-Swap
--  Am 24.12.2023 passiert Folgendes:
--  Ich bekomme 100 EUR
--  Ich bezahle 150 GBP

-- Sowas:
-- Am 13.05.2023 habe ich die Wahl zwischen:
-- ZCB 24.12.2023, 100 EUR
-- Currency-Swap ...

data Date = MkDate String
  deriving (Eq, Show, Ord)

type Amount = Float

data Currency = EUR | GBP | USD | YEN
  deriving Show

-- data Contract =
--     ZeroCouponBond Date Amount Currency
--     | CurrencySwap Date (Amount, Currency) (Amount, Currency)
--     -- müsste alles als einzelnen Fall aufnehmen
--     | ...
--     deriving (Show)

zcb :: Contract
zcb = AtDate (MkDate "24.12.2023") (Times 100 (One EUR))

makeZcb :: Date -> Amount -> Currency -> Contract
makeZcb d a c = AtDate d (Times a (One c))

-- >>> makeZcb (MkDate "24.12.2023") 100 EUR
-- AtDate (MkDate "24.12.2023") (Times 100.0 (One EUR))

-- AtDate 24.12.2023 (AtDate 6.10.2024 (Times 250 (One YEN)))

-- Elementare Bestandteile:
--  -- Menge
--  -- Währung
--  -- Datum
--  -- (Empfänger)

oneEuro :: Contract
oneEuro = One EUR

oneHundredEurosNow :: Contract
oneHundredEurosNow = Times 100 oneEuro

data Contract
    = One Currency    --- ich bekomme _jetzt_ 1 EUR
    | Times Amount Contract -- 100 * Vertrag
    | AtDate Date Contract  -- an 'Date' wird Vertrag fällig
    -- | All [Contract]
    | Both Contract Contract -- beides
    deriving Show

