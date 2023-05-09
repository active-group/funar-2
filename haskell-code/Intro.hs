module Intro where
import qualified Data.Sequence.Internal.Sorting as Arg

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
tupelFun :: (Int, Int) -> Int