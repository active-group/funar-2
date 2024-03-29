module Table(TableState, emptyTableState,
             runGame)
where

import qualified Data.Foldable as Foldable

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

import Cards
import GameEvent

import Debug.Trace (trace)

-- ist die Karte aus der Hand für den Stich zulässig ?
legalCard :: Card -> Hand -> Trick -> Bool
legalCard card hand trick =
  containsCard card hand &&
  (trickEmpty trick ||
   (let firstCard = leadingCardOfTrick trick
        firstSuit = suit firstCard
    in  suit card == firstSuit -- ok if suit is followed
        || all ((/= firstSuit) . suit) (handCards hand))) -- ok if no such suit in hand

-- Wert einer Karte
cardScore :: Card -> Integer
cardScore (Card Spades Queen) = 13
cardScore (Card Hearts _) = 1
cardScore _ = 0

-- * Spiellogik

-- Hände der Spieler
type PlayerHands = Map Player Hand

-- eingezogene Karten, pro Spieler
type PlayerPiles = Map Player Pile

data TableState =
  TableState
  { tableStatePlayers :: [Player], -- wer dran ist, steht vorn
    tableStateHands   :: PlayerHands,
    tableStatePiles  :: PlayerPiles,
    tableStateTrick   :: Trick
  }
  deriving Show

-- Anfangszustand herstellen
emptyTableState :: [Player] -> TableState
emptyTableState players =
  TableState {
    tableStatePlayers = players,
    tableStateHands = Map.fromList (map (\ player -> (player, emptyHand)) players),
    tableStatePiles = Map.fromList (map (\ player -> (player, emptyPile)) players),
    tableStateTrick = emptyTrick
  }

-- ist das Spiel noch am Anfang?
gameAtBeginning :: TableState -> Bool
gameAtBeginning tableState =
  (trickEmpty (tableStateTrick tableState)) && 
    (all pileEmpty (Map.elems (tableStatePiles tableState)))

-- wer ist als nächstes dran?
playerAfter :: TableState -> Player -> Player
playerAfter state player =
   head (rotate (rotateTo player (tableStatePlayers state)))

-- Liste rotieren
rotate :: [a] -> [a]
rotate (x : xs) = xs ++ [x]
rotate [] = undefined

-- Liste zu einem bestimmten Element rotieren
rotateTo :: Eq a => a -> [a] -> [a]
rotateTo y xs@(x : xs') | x == y = xs
                        | otherwise = rotateTo y (xs' ++ [x])
rotateTo y [] = undefined


-- wer ist gerade dran?
currentPlayer :: TableState -> Player
currentPlayer state =
  head (tableStatePlayers state)


-- ist es zulässig, diese Karte auszuspielen?
playValid :: TableState -> Player -> Card -> Bool
playValid tableState player card =
  let hand = tableStateHands tableState ! player
      trick = tableStateTrick tableState
  in
  legalCard card hand trick &&
  if gameAtBeginning tableState
  then card ==  Card Clubs Two
  else currentPlayer tableState == player

-- ist diese Runde vorbei?
turnOver :: TableState -> Bool
turnOver state =
  length (tableStatePlayers state) == length (trickToList (tableStateTrick state))

-- wer muß den Stich einziehen?
whoTakesTrick :: Trick -> Maybe Player
whoTakesTrick (Trick []) = Nothing
whoTakesTrick (Trick list) =
  case reverse list of 
    [] -> Nothing
    ((player0, card0) : rest') ->
      let loop player _ [] = Just player
          loop player card ((player', card') : rest) =
            case cardBeats card' card of
              Nothing -> loop player card rest
              Just False -> loop player card rest
              Just True -> loop player' card' rest
      in loop player0 card0 rest'

turnOverTrick :: TableState -> Maybe (Trick, Player)
turnOverTrick state =
  if turnOver state
  then
    let trick = tableStateTrick state
    in fmap (\player -> (trick, player)) (whoTakesTrick trick)
  else Nothing

-- Wert eines Stapels
pileScore :: Pile -> Integer
pileScore pile = sum (map cardScore (pileCards pile))

-- Ist das Spiel vorbei und wenn ja wer hat gewonnen?
gameOver :: TableState -> Maybe Player
gameOver state =
  if all isHandEmpty (Map.elems (tableStateHands state))
  then 
    let playerScores = Map.toList (fmap pileScore (tableStatePiles state))
        cmp (_, score1) (_, score2) = compare score1 score2
    in Just (fst (Foldable.minimumBy cmp playerScores))
  else 
    Nothing

-- Karte ausspielen
playCard :: PlayerHands -> Player -> Card -> PlayerHands
playCard playerHands player card =
  Map.alter (fmap (removeCard card)) player playerHands

-- Karten zum Stapel hinzufügen
addTrickToPile :: PlayerPiles -> Player -> Trick -> PlayerPiles
addTrickToPile playerPiles player trick =
  let playerPile = Map.findWithDefault emptyPile player playerPiles
  in Map.insert player (pileAddTrick playerPile trick) playerPiles

dealHand :: Player -> Hand -> PlayerHands -> PlayerHands
dealHand player hand hands = Map.insert player hand hands

-- Events in Zustand des Tisches einarbeiten
tableProcessEvent :: GameEvent -> TableState -> TableState
tableProcessEvent (HandDealt player hand) state =
  state { tableStateHands = dealHand player hand (tableStateHands state) }
tableProcessEvent (PlayerTurnChanged player) state =
  state { tableStatePlayers = rotateTo player (tableStatePlayers state) }
tableProcessEvent (LegalCardPlayed player card) state =
    state {
    tableStateHands = playCard (tableStateHands state) player card,
    tableStateTrick = addToTrick player card (tableStateTrick state)
  }
tableProcessEvent (TrickTaken player trick) state =
    state {
    tableStatePiles =
      addTrickToPile (tableStatePiles state) player trick,
    tableStateTrick = emptyTrick
  }
tableProcessEvent (GameEnded winner) state = state
tableProcessEvent (IllegalCardAttempted player card) state = state

-- (Purer) Interpreter für Spielablauf
-- --> mappt Konstruktoren von Game auf Logik
--         v  Spielablauf, der ausgeführt wird
--                   v  akt. Zustand
--                                 v  akt. Logbuch
--                                                v  Ergebniszustand
--                                                                v  Ergebnislogbuch
-- Ein anderes Spiel mit gleichem Ablauf -> anderer Interpreter
runGame :: Game a -> TableState -> [GameEvent] -> (TableState, [GameEvent], Either (GameCommand -> Game a) a)
runGame (IsCardValid player card callback) state events =
  runGame (callback (playValid state player card)) state events
runGame (TurnOverTrick callback) state events =
  runGame (callback (turnOverTrick state)) state events
runGame (PlayerAfter player callback) state revents =
  runGame (callback (playerAfter state player)) state revents
runGame (IsGameOver callback) state revents =
  runGame (callback (gameOver state)) state revents

-- fehlt noch: RecordEvent, Done, WaitForCommand
runGame (Done result) state events =
  (state, reverse events, Right result)
runGame (RecordEvent evt callback) state events =
  let newState = tableProcessEvent evt state
  in runGame (callback ()) newState (evt : events)
runGame (WaitForCommand callback) state events =
  (state, reverse events, Left callback)