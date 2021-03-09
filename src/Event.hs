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

instance FromJSON Text where
    fromJSON (Str xs) = Just $ pack xs
    fromJSON _ = Nothing

instance FromJSON Event where
    fromJSON (Obj o) = do
        i <- lookup "id" o
        name <- lookup "title" o
        when <- lookup "when" o
        loc <- lookup "where" o

        i2 <- fromJSON i
        name2 <- fromJSON name
        when2 <- fromJSON when
        loc2 <- fromJSON loc
        
        pure $ Event i2 name2 when2 loc2

--------------------------------------------------------------------------------
