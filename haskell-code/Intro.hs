{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

-- Signatur
x :: Integer
-- mathematische Gleichung
x = 7

y :: Integer
y = 12 * x + 3

-- Funktionen in Haskell
f :: Integer -> Integer
f = \n -> n * 2 -- Lambda in Haskell

-- Alternative Schreibweise
f' :: Integer -> Integer
f' n = n * 2

-- Kleinbuchstaben für Funktionen/Werte
-- Großbuchstaben für Typen

-- Ein Haustier ist eins der Folgenden:
-- - Katze -ODER-
-- - Hund -ODER-
-- - Schlange
data Pet = Cat | Dog | Snake
--   ^ Datentyp
--          ^  Werte

-- Zwei Pets miteinander vergleichen
eqPet :: Pet -> (Pet -> Bool)
eqPet Cat Cat = True
eqPet Dog Dog = True
eqPet Snake Snake = True
-- _  -> "don't care"
eqPet _ _ = False

-- totale Funktionen
-- für jeden Input gibt es immer einen Output
foo :: Integer -> Integer
foo 3 = 17
foo 4 = 5
-- hier habe ich einen Namen vergeben
foo n = n - 1

-- Ist ein Haustier niedlich?
isCute :: Pet -> Bool
isCute Cat = True
isCute Dog = True
isCute Snake = False

-- >>> isCute Snake == False
-- True

-- Ein Gürteltier hat folgende Eigenschaften:
-- - lebendig oder tot
-- - Gewicht
data Liveness = Dead | Alive
  deriving (Show, Eq)

-- Typalias oder -synonym
type Weight = Integer

--           v Konstruktor
-- data Dillo = MkDillo { dilloLiveness :: Liveness, -- Selektor
--                        dilloWeight :: Weight }
--   deriving (Eq, Show)

dillo1 :: Animal
dillo1 = MkDillo Alive 10

dillo2 :: Animal
-- dillo2 = MkDillo Dead 8
dillo2 = MkDillo {dilloLiveness = Dead, dilloWeight = 8}

-- Dillo überfahren
-- runOverDillo :: Dillo -> Dillo

-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)

-- runOverDillo dillo = dillo{dilloLiveness = Dead} -- funktionales Update

-- runOverDillo (MkDillo {dilloWeight = w}) =
--     MkDillo Dead w

-- runOverDillo dillo@(MkDillo {dilloLiveness = Dead}) =
--     dillo
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)

-- runOverDillo (MkDillo _ w) = MkDillo Dead w

-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverDillo dillo2
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}

type Sentence = String

-- Tier ist eins der Folgenden:
-- - Gürteltier
-- - Papagei
-- tagged union types
-- discriminated union types
-- sum types / Summentypen
data Animal =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  | MkParrot Sentence Weight
  deriving (Eq, Show)

parrot1 :: Animal
parrot1 = MkParrot "Hi" 1

parrot2 :: Animal
parrot2 = MkParrot "Ciao" 2

-- Tiere überfahren
runOverAnimal :: Animal -> Animal
runOverAnimal (MkDillo _ w) = MkDillo Dead w
runOverAnimal (MkParrot _ w) = MkParrot "" w


-- Alternative Modellierung
data Dillo' = MkDillo' Liveness Weight
data Parrot' = MkParrot' Sentence Weight

data Animal' =
    DilloA Dillo' | ParrotA Parrot'

-- runOverAnimal' :: Animal' -> Animal'
-- runOverAnimal' (DilloA dillo) = runOverDillo dillo
-- runOverAnimal' (ParrotA parrot) = runOverParrot parrot

-- Currying: Haskell ist implizit gecurryt
-- mehrstellige Funktionen sind eigentlich einstellig,
-- liefern aber Funktionen zurück, die die restlichen Arg. nehmen

-- Higher-Order-Funktion:
-- - Funktionen, die Funktionen als Argumente nehmen
-- - Funktionen, die Funktionen zurückgeben
-- - oder beides

-- doSomething :: Config -> (Int -> String)
-- doSomething config n = blabla n + timeout config

-- listMap (\x -> doSomething config x) [x1, x2, x3]
-- listMap (doSomething config) [x1, x2, x3]


-- BBB: https://bbb.active-group.de/b/joh-nn2-wlw-wp2

-- Currying

-- Tupel
tup :: (String, Integer, Integer -> Integer)
tup = ("abc", 17, \n -> n - 3)

tupelFun :: (Int, Int) -> Int
-- tupelFun (n, m) = n + m + 3
tupelFun t = fst t + snd t + 3

--          v  Typvariable (%a in Racket)
tuplify :: (a -> b -> c) -> ((a, b) -> c)
-- tuplify    f              = \(a, b) -> (f a) b
tuplify = \f -> (\(a, b) -> f a b)

-- (tuplify (+)) (2, 3)

untuplify :: ((a, b) -> c) -> (a -> b -> c)
untuplify f = \a b -> f (a, b)

-- curry / uncurry eingebaut

-- Duschprodukte
-- - Seife (hat pH-Wert)
-- - Shampoo (ist geeignet für Haartyp)
-- - Duschgel (50% Seife, 50% Shampoo)

-- 1) Datenanalyse + Datentyp (s. Animal)
-- 2) Funktion, die den Seifenanteil berechnet

-- 3) Erweiterung des Datentyps:
--    - Mixtur aus zwei Duschprodukten, bel. Anteile
--    - Funktion anpassen

-- https://teams.microsoft.com/l/meetup-join/19%3ameeting_ODgxOGExN2YtZjAwYy00MTliLWEyNjktZjJjMWY5NGM2NjE4%40thread.v2/0?context=%7b%22Tid%22%3a%2282bddc65-9759-44b2-af74-ec76b2e6812a%22%2c%22Oid%22%3a%22526ccabf-c09c-4f80-90f3-fb7a4f809318%22%7d

-- Listen

data ListOf element =
    Empty
  | Cons element (ListOf element)

list1 :: ListOf Integer
list1 = Cons 5 Empty
-- ....

-- Eingebauter Typ: []
-- Datenkonstruktor:    :
-- leere Liste: []
list1' :: [Integer]
list1' = 5 : []  -- 5 cons leere Liste
-- list1' = (:) 5 []

list2 :: [Integer]
list2 = 1 : 2 : []

list3 :: [Integer]
list3 = [1,2,3]

listSum :: [Integer] -> Integer
listSum [] = 0 -- neutrales Element
listSum (n : rest) = n + listSum rest

listFold :: acc -> (b -> acc -> acc) -> [b] -> acc
listFold neutral op [] = neutral
listFold neutral op (x : xs) =
    op x (listFold neutral op xs)

-- Sieb des Eratosthenes (Primzahlen ermitteln)
-- 2 3 4 5 ...
-- 2 3  5  7  9  11 ...
-- 2 3  5  7    11 ...
-- 2 3  5  7    11   13  17  ...

-- lazy evaluation (vs. strikte Auswertung)
-- strikt: bei einem Funktionsaufruf werden zuerst Arg. ausgewertet
-- Haskell:
-- Argumente werden erst ausgewertet, wenn ich sie benötige

-- Alle nat. Zahlen ab einer bestimmten
natsFrom :: Integer -> [Integer]
natsFrom n = n : natsFrom (n + 1)

-- Streichen von Vielfachen einer Zahl
strikeMultiples :: Integer -> [Integer] -> [Integer]
strikeMultiples n list =
    filter (\m -> mod m n /= 0) list
-- >>> strikeMultiples 2 [1 .. 10]
-- [1,3,5,7,9]

-- Primzahlsieb
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (p : rest) =
    p : sieve (strikeMultiples p rest)

data Optional a =
    Result a
    | Null
    deriving (Show)

-- >>> safeDivide 3 5
-- Result 0.6
-- >>> safeDivide 3 0
-- Null
safeDivide :: Double -> Double -> Optional Double
safeDivide x y =
    if y == 0
        then Null
        else Result (x / y)

doSomething x =
    -- if 3 `safeDivide` x > 2 then 5 else 7
    case 3 `safeDivide` x of
        Null -> 7
        Result res -> if res > 2 then 5 else 7

-- Typklassen
-- class Eq a where
--     -- Methoden
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool

-- bisher:
-- Eq: Gleicheit
-- Show: 'Ausdrucken'
-- Num: Zahlen bzw. numerische Operationen
-- class Num a where
--   (+) :: a -> a -> a
--   (-) :: a -> a -> a
--   (*) :: a -> a -> a
--   negate :: a -> a
--   abs :: a -> a
--   signum :: a -> a
--   fromInteger :: Integer -> a


-- In FP:
-- Typklassen werden idR für "universelle" Abstraktionen verwendet

{-

Algebra:
- Typ T
- Operationen (mit Typsignaturen)
- Gesetze / Gleichungen

Addition:
x + 0 = 0 + x = x     "0 ist neutral bzgl. Addition"

Listen:
list1 `concat` [] = [] `concat` list1 = list1

-}

-- Typ T
-- Operation:  op ::  T -> T -> T
-- Assoziativgesetz:  op a (op b c) == op (op a b) c
-- Assoziativgesetz:  a `op` (b `op` c) == (a `op` b) `op` c

-- alg. Struktur:   Halbgruppe

-- "a erfüllt das Interface Semigroup"
class Semigroup a where
    -- Methoden "Kombinator"   (G x G -> G)
    op :: a -> a -> a
    -- fordern noch: assoziativ

-- "Interface implementieren"
-- --> instance
instance Semigroup [a] where
    op list1 list2 = list1 ++ list2  -- Listenkonkatenation

-- funktioniert schon für String: :info String!

-- Monoid als "Interface" "erbt" von Semigroup
class Semigroup t => Monoid t where
    -- Gesetz:   neutral `op` x == x == x `op` neutral
    neutral :: t

instance Monoid [a] where
    neutral = []

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
    -- op :: (a, b) -> (a, b) -> (a, b)
    op :: (Semigroup a, Semigroup b) => (a, b) -> (a, b) -> (a, b)
    op (a1, b1) (a2, b2) = (op a1 a2, op b1 b2)

instance (Monoid a, Monoid b) => Monoid (a, b) where
    -- neutral :: (a, b)
    -- (a,b) `op` neutral == (a,b)
    -- (a,b) `op` (x, y) == (op a x, op b y) == (a,b)
    neutral = (neutral, neutral)

-- Stringtypen:
-- String, Text, LazyText, ByteString, LazyByteString

-- Mit String konkret:
-- if True then myString ++ someOtherString else ""
-- Ich möchte Text verwenden:
-- if True then Text.concat myString someOtherString else Text.empty

-- Stattdessen:
-- if True then myString `op` someOtherString else neutral

-- Aufgabe:
instance Semigroup a => Semigroup (Optional a) where
    -- op :: Optional a -> Optional a -> Optional a
    op Null (Result a) = Result a -- Null
    op (Result a) Null = Result a -- Null
    op Null Null = Null
    op (Result a1) (Result a2) = Result (a1 `op` a2)

-- Bonus:
instance Monoid a => Monoid (Optional a) where
    -- neutral :: Optional a
    -- op neutral x == x
    -- neutral = Result ???
    neutral = Null