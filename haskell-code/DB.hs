module DB where

{-

Wollen: Key-Value-Store verwenden
Key = String, Value = Int

Würde gerne schreiben: "Datenbankprogramm"

  put "Johannes" 36
  x = get "Johannes"
  put "Johannes" (x + 1)
  y = get "Johannes"
  return (show (x + y))

-}

-- Liste von Kommandos?

type Key = String

-- data Animal = MkParrot Sentence Weight | MkDillo Liveness Weight
-- Datenbankkommando
-- data DBCommand a =
--     Put Key Int
--     | Get Key
--     | Return a

-- DB-Programm mit Rückgabetyp a
-- type DBProgram a = [DBCommand a]

--                       v Ergebnis?  Wie Namen geben?
-- p1 = [Put "Johannes" 36, Get "Johannes", Put "Johannes" ???]

-- Liste ist futsch

-- Idee: Abläufe als modellierter Wert!


-- Beschreibung eines Datenbankprogramms mit Ergebnistyp a
data DB a =
    -- Callback
    -- Wer das "Get" schreibt, definiert den Callback und damit
    -- die Fortsetzung der Logik
    -- Get "Johannes" (\x -> ... -- Restablauf)
    Get Key (Int -> DB a) -- Selbstbezug!
    -- Fortsetzung == Callback
  | Put Key Int (() -> DB a)  -- () heißt Unit (denkt: void)
  | Return a

p1 :: DB String
p1 =
    Put "Johannes" 36 (\() ->
        Get "Johannes" (\x ->
            Put "Johannes" (x + 1) (\() ->
                Get "Johannes" (\y ->
                    Return (show (x + y))))))

-- Einfache Datenbankprogramme:
-- 1) Gib einen festen Wert zurück (ohne in die DB zu schauen)
-- 2) Schreibe einen Wert und sei fertig
-- 2) Lies einen Wert und sei fertig

-- primitive Hilfsfunktion / Hilfsprogramm
put :: Key -> Int -> DB () -- "void"
put key val = Put key val (\() -> Return ()) -- brauche: DB ()

get :: Key -> DB Int
-- get key = Get key (\result -> Return result) 
get key = Get key Return -- tricky Schreibweise

-- Programm, das nichts tut und gleich terminiert mit dem geg. Wert
-- return :: a -> DB a
-- return = Return

step1 :: DB ()
step1 = put "Johannes" 36

step2 :: DB Int
step2 = get "Johannes"

-- Seil-Analogie:  jedes Programm "ist ein Seil"
-- jedes Seil hat ein Ende
-- am Ende steht immer "Return"

-- Programme zusammenspleißt
--                   v    hat keinen Zugriff auf des Erg. des ersten Ablaufs
-- splice :: DB a -> DB b -> DB b
splice :: DB a -> (a -> DB b) -> DB b
splice (Get key callback) next =
    Get key (\value -> splice (callback value) next)
splice (Put key value callback) next =
    Put key value (\() -> splice (callback ()) next)
-- Das Ende des Seils -> hier wird gearbeitet
splice (Return result) next = next result

p1' :: DB String
p1' =
    splice (put "Johannes" 36) (\() ->
    splice (get "Johannes") (\x ->
    splice (put "Johannes" (x+1)) (\() ->
    splice (get "Johannes") (\y ->
    Return (show (x+y))))))

-- Syntaktischer Zucker für p1'
p1'' :: DB String
p1'' = do -- do-Notation
    put "Johannes" 36   -- "monadischer Wert"
    x <- step2
    -- put "Johannes" (x+1)
    -- y <- step2
    y <- increaseAge "Johannes"
    Prelude.return (show (x + y))

--          > m "fehlt noch ein Argument"
class Applicative m => MyMonad m where
    -- "bind"
    --           v  Typkonstruktor / Funktion auf Typebene
    -- splice :: DB a -> (a -> DB b) -> DB b
    (>>=) ::     m  a -> (a -> m  b) -> m  b

-- Dinge, über die ich "mappen" kann
class MyFunctor m where
    -- map :: (a -> b) -> [a] -> [b]
    -- map :: (a -> b) -> List a -> List b
    map' ::   (a -> b) -> m a    -> m b

data Optional a = Result a | Null

instance MyFunctor Optional where
    -- map' :: (a -> b) -> Optional a -> Optional b
    map' f Null = Null
    map' f (Result x) = Result (f x)
    -- heißt eigentlich:  fmap

instance Functor DB where
instance Applicative DB where

--             v   _nicht_ DB a
instance Monad DB where
    (>>=) = splice

-- Alter erhöhen      v    ich bin "in der Monade"
increaseAge :: Key -> DB Int
increaseAge key = do
    age <- get key
    let newAge = age + 1
    put key newAge
    return newAge