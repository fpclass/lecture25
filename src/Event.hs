--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Writing a real application in Haskell                             --
--------------------------------------------------------------------------------

module Event where

--------------------------------------------------------------------------------

import Data.Text

import System.IO

import JSON

--------------------------------------------------------------------------------

data Event = Event {
    eventID       :: Integer,
    eventName     :: Text,
    eventDateTime :: Text,
    eventWhere    :: Text
} deriving Show

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
