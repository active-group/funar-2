module Contracts where

-- Zero-Coupon-Bond, einfaches Beispiel für Vertrag
-- "Ich bekomme am 24.12.2023 100 Euro."

data Date = MkDate String
  deriving (Eq, Show, Ord)

data 