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
    | RecordEvent GameEvent (() -> Game a)
    | TurnOverTrick (Maybe (Player, Trick) -> Game a)
    | Done a

instance Functor Game where
instance Applicative Game where

instance Monad Game where
    -- (>>=) :: Game a -> (a -> Game b) -> Game b
    (>>=) (Done a) next = next a
    (>>=) (IsCardValid player card callback) next =
        IsCardValid player card (\b -> (>>=) (callback b) next)
    (>>=) (RecordEvent evt callback) next =
        RecordEvent evt (\b -> (>>=) (callback b) next)
    return = Done

isCardValidM :: Player -> Card -> Game Bool
isCardValidM player card = IsCardValid player card (\b -> Done b)

recordEventM :: GameEvent -> Game ()
recordEventM evt = RecordEvent evt Done

turnOverTrickM :: GameEvent -> Game ()
turnOverTrickM evt = TurnOverTrick Done

-- Ergebnis: der Ablauf, der passiert, wenn das Command behandelt wird
tableProcessCommand :: GameCommand -> Game (Maybe Player)
tableProcessCommand (DealHands hands) = undefined
tableProcessCommand (PlayCard player card) = do
    isCardValid <- isCardValidM player card
    if isCardValid
        then do
            recordEventM (LegalCardPlayed player card)
            turnOverTrick <- turnOverTrickM
        else do 
            recordEventM (IllegalCardAttempted player card)
            return Nothing -- Nothing == Null