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
    -- Get "Johannes" (\value -> ...)
    Get Key (Int -> )
  | Put Key Int
  | Return a