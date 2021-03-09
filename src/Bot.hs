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

import System.IO

import qualified Data.Map as M

--------------------------------------------------------------------------------

cs141botUsername :: String
cs141botUsername = "<@UFUR72ZEC>"

lecturesChannel :: String
lecturesChannel = "CAQ5FQX89"

handler :: M.Map Integer Event -> SlackEvent -> IO SlackResponse
handler _ Connected = do
    putStrLn "Connected to Slack!"
    return NoResponse
handler events (Received msg) = do
    case parse nat msg of 
        Nothing -> putStrLn "No parse." >> return NoResponse
        Just (i,_) ->
            case M.lookup (fromInteger i) events of
                Nothing -> do
                    putStrLn "No such event."
                    return (SendMessage lecturesChannel "No such event!")
                Just event -> do 
                    return (SendMessage lecturesChannel (show event))


runBot :: IO ()
runBot = withFile "events.json" ReadMode $ \h -> do
    xs <- hGetContents h
    Just token  <- lookupEnv "SLACK_API_TOKEN"
    let Just events = parseJSON xs 

    runSlackBot token (handler $ M.fromList $ [ (eventID x, x) | x <- events ]) 

--------------------------------------------------------------------------------
