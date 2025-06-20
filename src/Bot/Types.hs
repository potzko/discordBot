{-# LANGUAGE OverloadedStrings #-}
module Bot.Types where

import Discord
import Discord.Types
import qualified Data.Text as T

type MessagePredicate state = state -> T.Text -> Bool
type MessageAction state = Event -> state -> DiscordHandler ()

data BotAction state = BotAction
  { matchMsg  :: MessagePredicate state
  , runAction :: MessageAction state
  }

data GlobalState = GlobalState
