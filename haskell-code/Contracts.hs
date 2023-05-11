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
    | Negate Contract
    | Empty
    deriving Show

currencySwap :: Date -> (Amount, Currency) -> (Amount, Currency) -> Contract
currencySwap date (myAmount, myCurrency) (theirAmount, theirCurrency) =
    -- AtDate date (Both (Times myAmount (One myCurrency))
    --                   (Negate (Times theirAmount (One theirCurrency))))
    Both (makeZcb date myAmount myCurrency)
         (Negate (makeZcb date theirAmount theirCurrency))

-- im Paper:
-- scale :: Obs Float -> Contract -> Contract

-- Interpreter für Verträge
-- Sichtweise: Wer bekommt wann wieviel Geld?
-- Konkreter:  Wenn ich den Vertrag heute anschauen, 
-- welche Zahlungen entstehen daraus?

-- Haben: Syntax (Contract)
-- Brauchen: Semantik(en)

data Direction = ForMe | ForSomeoneElse -- Long | Short
    deriving Show

data Payment = Payment Direction Date Amount Currency
    deriving Show

negatePayment :: Payment -> Payment
negatePayment (Payment ForMe d a c) = Payment ForSomeoneElse d a c
negatePayment (Payment ForSomeoneElse d a c) = Payment ForMe d a c

-- Welche Zahlungen entstehen, wenn ich den Vertrag am Datum x anschaue?
--                       v   "jetzt"
--                               v   Restvertrag (was ist noch zu erledigen?)
--                                          v   resultierende Zahlungen
semantics :: Contract -> Date -> (Contract, [Payment])
semantics (One curr) now = 
    (Empty, [Payment ForMe now 1 curr])
semantics (Negate inner) now =
    let (restContract, payments) = semantics inner now
     in (Negate restContract, map negatePayment payments)
semantics (Both c1 c2) now = undefined
semantics (Times amount inner) now =
    let (restContract, payments) = semantics inner now
     in (Times amount inner, map (scalePayment amount) payments)
semantics contract@(AtDate date inner) now =
    if date > now
        then (contract, [])  -- es passiert noch nichts
        else semantics inner now  -- inneren Vertrag auswerten
semantics Empty _ = (Empty, [])