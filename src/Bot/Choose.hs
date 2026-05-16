{-|
Module      : Bot.Choose
Description : Implements "choose A | B | C" (random pick) and "coinflip"
              (which is just choose heads/tails under the hood).
-}

{-# LANGUAGE OverloadedStrings #-}
module Bot.Choose (chooseAction, coinflipAction) where

import Discord.Types
import Control.Monad (void, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import System.Random (randomRIO)
import Bot.Types
import Bot.Util (sendMessageSafe)

-- | Pick a uniformly random element. Caller must ensure the list is non-empty.
pickRandom :: [a] -> IO a
pickRandom [] = error "pickRandom: empty list"
pickRandom xs = do
  i <- randomRIO (0, length xs - 1)
  return (xs !! i)

chooseAction :: BotAction GlobalState
chooseAction = BotAction
  { botActionName = "Choose"
  , matchMsg = \_ txt -> "choose " `T.isPrefixOf` T.toLower txt
  , runAction = \event _ -> case event of
      MessageCreate msg -> unless (userIsBot (messageAuthor msg)) $ do
        let content = T.strip $ T.dropWhile (/= ' ') (messageContent msg)
            options = filter (not . T.null) (map T.strip (T.splitOn "|" content))
        case options of
          []  -> void $ sendMessageSafe "Choose" (messageChannelId msg)
                   "Usage: choose A | B | C"
          [_] -> void $ sendMessageSafe "Choose" (messageChannelId msg)
                   "Need at least two options separated by `|`."
          _   -> do
            picked <- liftIO (pickRandom options)
            void $ sendMessageSafe "Choose" (messageChannelId msg) picked
      _ -> return ()
  , actionMemBudget = Nothing
  }

coinflipAction :: BotAction GlobalState
coinflipAction = BotAction
  { botActionName = "Coinflip"
  , matchMsg = \_ txt -> "coinflip" `T.isPrefixOf` T.toLower (T.strip txt)
  , runAction = \event _ -> case event of
      MessageCreate msg -> unless (userIsBot (messageAuthor msg)) $ do
        result <- liftIO (pickRandom ["heads", "tails"])
        void $ sendMessageSafe "Coinflip" (messageChannelId msg) result
      _ -> return ()
  , actionMemBudget = Nothing
  }
