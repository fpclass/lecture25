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
    eventID       :: Int,
    eventName     :: String,
    eventDateTime :: String,
    eventWhere    :: String
} deriving Show

goodName :: FromJSON a => String -> Object -> Maybe a 
goodName key obj = lookup key obj >>= fromJSON

instance FromJSON Event where 
    fromJSON (Obj o) = Event <$> goodName "id" o 
                             <*> goodName "title" o
                             <*> goodName "when" o 
                             <*> goodName "where" o

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
