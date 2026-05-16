{-# LANGUAGE OverloadedStrings #-}
module Bot.PingPong (pingAction, pongAction) where

import Discord.Types
import Control.Monad (void)
import qualified Data.Text as T
import Bot.Util (sendMessageSafe)

import Bot.Types

pingAction :: BotAction GlobalState
pingAction = BotAction
  { botActionName = "PingPong:Ping"
  , matchMsg = \_ txt -> "ping" `T.isPrefixOf` txt
  , runAction = \event _ -> case event of
      MessageCreate msg ->
        void $ sendMessageSafe "PingPong:Ping" (messageChannelId msg) "Pong!"
      _ -> return ()
  , actionMemBudget = Nothing
  }

pongAction :: BotAction GlobalState
pongAction = BotAction
  { botActionName = "PingPong:Pong"
  , matchMsg = \_ txt -> "pong" `T.isPrefixOf` txt
  , runAction = \event _ -> case event of
      MessageCreate msg ->
        void $ sendMessageSafe "PingPong:Pong" (messageChannelId msg) "Ping!"
      _ -> return ()
  , actionMemBudget = Nothing
  }
