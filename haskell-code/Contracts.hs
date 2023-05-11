module Contracts where

-- Zero-Coupon-Bond, einfaches Beispiel für Vertrag
-- "Ich bekomme am 24.12.2023 100 Euro."

data Date = MkDate String
  deriving (Eq, Show, Ord)

type Amount = Float

data Currency = EUR | GBP | USD | YEN
  deriving Show

data Contract =
    ZeroCouponBond Date Amount Currency
    deriving (Show)