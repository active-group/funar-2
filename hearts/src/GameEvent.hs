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
data GameEvent
    = GameEnded Player -- Gewinner
    | GameStarted Player
    -- | CardsShuffled
    | CardsDistributed Player Hand
    | CardPlayed Player Card
    | TrickTaken Player Trick
    | PlayerTurnChanged Player
    | IllegalCardAttempted Player Card

data GameCommand
    = PlayCard Player Card
    | DealHands (Map Player Hand)