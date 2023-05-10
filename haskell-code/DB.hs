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
get key = Get key (\result -> Return result) 

return :: a -> DB a
return a = Return a