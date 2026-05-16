{-# LANGUAGE OverloadedStrings #-}
module Bot.Types (GlobalState(..), BotAction(..), MessagePredicate, MessageAction) where

import Discord
import Discord.Types
import Data.Int (Int64)
import qualified Data.Text as T

type MessagePredicate state = state -> T.Text -> Bool
type MessageAction state = Event -> state -> DiscordHandler ()

data BotAction state = BotAction
  { botActionName   :: String
  , matchMsg        :: MessagePredicate state
  , runAction       :: MessageAction state
  -- | Per-action allocation budget override. Nothing means "use the global
  --   default in Main." Set this when an action legitimately needs more
  --   headroom than the default (e.g. Factorize).
  , actionMemBudget :: Maybe Int64
  }

data GlobalState = GlobalState
