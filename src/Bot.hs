--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Writing a real application in Haskell                             --
--------------------------------------------------------------------------------

module Bot where

--------------------------------------------------------------------------------

import System.Environment (lookupEnv)

import Event
import Parser
import JSON
import BotUtil

--------------------------------------------------------------------------------

cs141botUsername :: String
cs141botUsername = "<@UFUR72ZEC>"

lecturesChannel :: String
lecturesChannel = "CAQ5FQX89"

runBot :: IO ()
runBot = return ()

--------------------------------------------------------------------------------
