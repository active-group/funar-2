module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Events: Log von Dingen, die in der Anwendung passieren
-- vollständig! enthält alles, was passiert ist

-- -> Erzählung

-- (kein) Event-Sourcing: Events als Quelle der Wahrheit
-- ES ist Persistenz

-- Events:
-- - liegen in der Vergangenheit
-- - enthält keinen Zustand
-- - vollständig
-- - fachlich motiviert / orientiert
-- - Redundanz ist OK

-- Commands:
-- Bitte, dass etwas passieren soll _in der Zukunft_

-- Was passiert bei Hearts?
-- data GameEvent
--     = GameEnded Player -- Gewinner
--     | GameStarted Player -- optional, weil Kreuz 2 beginnt
--     -- | CardsShuffled
--     | CardsDistributed Player Hand
--     | CardPlayed Player Card
--     | TrickTaken Player Trick
--     | PlayerTurnChanged Player
--     | IllegalCardAttempted Player Card

data GameCommand
    = PlayCard Player Card
    | DealHands (Map Player Hand)

data GameEvent
    = HandDealt Player Hand
    | PlayerTurnChanged Player
    | LegalCardPlayed Player Card
    | TrickTaken Player Trick
    -- könnte auch zweiteilen in TrickClosed und TrickTaken
    | GameEnded Player
    | IllegalCardAttempted Player Card
    deriving Show


-- Wollen Spielablauf modellieren!
-- Ein Hearts-Programm mit Ergebnis a
data Game a =
    IsCardValid Player Card (Bool -> Game a)
    | Done a

instance Monad Game where
    -- (>>=) :: Game a -> (a -> Game b) -> Game b
    (>>=) (Done a) next = next a
    return = Done

isCardValidM :: Player -> Card -> Game Bool
isCardValidM = undefined

-- Ergebnis: der Ablauf, der passiert, wenn das Command behandelt wird
tableProcessCommand :: GameCommand -> Game (Maybe Player)
tableProcessCommand (DealHands hands) = undefined
tableProcessCommand (PlayCard player card) = do
    isCardValid <- isCardValidM player card
    if isCardValid
        then undefined
        else undefined