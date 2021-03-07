--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Writing a real application in Haskell                             --
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module REPL where

--------------------------------------------------------------------------------

import qualified Data.Map as M

import JSON
import Event

import System.IO

--------------------------------------------------------------------------------

type Events = M.Map Int Event

myEvents :: Events
myEvents = M.fromList [
    (0, Event 0 "FP Thursdays" "Tomorrow at 7pm" "YouTube")
 ,  (1, Event 1 "FP Gaming" "Friday at 7pm" "Steam?")
 ]

repl :: IO ()
repl = do
    putStr "Enter an ID: "
    eventID <- read <$> getLine

    case M.lookup eventID myEvents of 
        Nothing -> putStrLn "Event not found!"
        Just e -> print e

    repl

--------------------------------------------------------------------------------
