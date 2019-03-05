--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 25: Fun with IO                                                    --
--------------------------------------------------------------------------------

-- | A simplified library for writing Slack bots.
module BotUtil (
    SlackEvent(..),
    SlackResponse(..),
    runSlackBot
) where

--------------------------------------------------------------------------------

import Control.Concurrent
import Control.Monad
import Data.Text
import Web.Slack

--------------------------------------------------------------------------------

data SlackEvent
    = Connected
    | Received String
    deriving Show

data SlackResponse
    = NoResponse
    | SendMessage String String
    deriving Show

--------------------------------------------------------------------------------

-- | `toSlackEvent` @event@ converts @event@ into a simpler representation.
toSlackEvent :: Event -> Maybe SlackEvent
toSlackEvent Hello = pure Connected
toSlackEvent (Message cid who msg _ _ _) = pure $ Received (unpack msg)
toSlackEvent _ = Nothing

-- | `bootLoop` @handler handle@ runs the main loop for a bot using a connection
-- represented by @handle@, dispatching events to @handler@ when they fire.
botLoop :: (SlackEvent -> IO SlackResponse) -> SlackHandle -> IO ()
botLoop handler slack = forever $ do
    event <- getNextEvent slack
    case toSlackEvent event of
        Nothing -> return ()
        Just e  -> do
            response <- handler e
            case response of
                NoResponse -> return ()
                SendMessage cid msg -> do
                    sendMessage slack (Id $ pack cid) (pack msg)
                    -- in anticipation of people spamming the Slack channel,
                    -- we rate-limit ourselves to one message per second
                    threadDelay 1000000

-- | `runSlackBot` @token handler@ connects to the Slack RTM API using @token@
-- and uses @handler@ to handle events.
runSlackBot :: String -> (SlackEvent -> IO SlackResponse) -> IO ()
runSlackBot token handler = withSlackHandle config (botLoop handler)
     where config = SlackConfig { _slackApiToken = token }

--------------------------------------------------------------------------------
