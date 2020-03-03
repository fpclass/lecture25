--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Fun with IO                                                       --
--------------------------------------------------------------------------------

module Event where

--------------------------------------------------------------------------------

import Data.Text

import System.IO

import JSON

--------------------------------------------------------------------------------

data Event = Event {
    eventID       :: Integer,
    eventName     :: String,
    eventDateTime :: String,
    eventWhere    :: String
} deriving Show

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
