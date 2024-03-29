module Main where

import           Args         (Args (Args), ServerType (..), parseArgs)
import qualified PlayerServer
import qualified TableServer

main :: IO ()
main = do
  Args port serverType playerName <- parseArgs
  case serverType of
    Player -> PlayerServer.runServer port playerName
    Table  -> TableServer.runServer port
