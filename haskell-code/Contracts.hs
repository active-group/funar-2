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
zcb = ZeroCouponBond (MkDate "24.12.2023") 100 EUR

-- Elementare Bestandteile:
--  -- Menge
--  -- Währung
--  -- Datum
--  -- Empfänger

data Contract
    = 