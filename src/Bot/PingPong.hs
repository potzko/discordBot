{-# LANGUAGE OverloadedStrings #-}
module Bot.PingPong (pingAction, pongAction) where

import Discord
import Discord.Types
import Discord.Requests
import Control.Monad (void)
import qualified Data.Text as T

import Bot.Types

pingAction :: BotAction GlobalState
pingAction = BotAction
  { matchMsg = \_ txt -> "ping" `T.isPrefixOf` txt
  , runAction = \event _ -> case event of
      MessageCreate msg ->
        void $ restCall (CreateMessage (messageChannelId msg) "Pong!")
      _ -> return ()
  }

pongAction :: BotAction GlobalState
pongAction = BotAction
  { matchMsg = \_ txt -> "pong" `T.isPrefixOf` txt
  , runAction = \event _ -> case event of
      MessageCreate msg ->
        void $ restCall (CreateMessage (messageChannelId msg) "Ping!")
      _ -> return ()
  }
