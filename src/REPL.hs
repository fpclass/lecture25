--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Fun with IO                                                       --
--------------------------------------------------------------------------------

module REPL where

--------------------------------------------------------------------------------

import JSON
import Event

import System.IO

--------------------------------------------------------------------------------

type Events = [(Int, Event)]

-- myEvents :: Events 
-- myEvents = [(1, Event 1 "test event" "not today" "hell, probably")]

loadEvents :: FilePath -> IO (Maybe [Event])
loadEvents fp = do 
    mVal <- parseFile fp

    pure (mVal >>= fromJSON)

loop :: Events -> IO ()
loop events = do  
    putStr "Please enter an event ID: "
    eventID <- read <$> getLine 

    case lookup eventID events of 
        Nothing -> putStrLn "Event not found."
        Just e  -> print e

    loop events

repl :: IO ()
repl = loop undefined


--------------------------------------------------------------------------------
