{-# LANGUAGE OverloadedStrings #-}
module Bot.Types (GlobalState(..), BotAction(..), MessagePredicate, MessageAction) where

import Discord
import Discord.Types
import qualified Data.Text as T

type MessagePredicate state = state -> T.Text -> Bool
type MessageAction state = Event -> state -> DiscordHandler ()

data BotAction state = BotAction
  { botActionName :: String
  , matchMsg  :: MessagePredicate state
  , runAction :: MessageAction state
  }

data GlobalState = GlobalState
