{-# LANGUAGE OverloadedStrings #-}
module Bot.FibAction (fibAction) where

import Discord.Types
import Control.Monad (void, unless)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Bot.Types
import Bot.Util (sendMessageSafe)

-- Fast doubling Fibonacci implementation with support for negative indices
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib a
  | a < 0 && odd a  = fib (-a)
  | a < 0           = -fib (-a)
  | odd a           = fibHalf ^ (2 :: Integer) + (fibHalf + fibHalfPred) ^ (2 :: Integer)
  | otherwise       = fibHalf * (fibHalf + 2 * fibHalfPred)
  where
    half = div a 2
    fibHalf = fib half
    fibHalfPred = fib (pred half)

-- Discord command handler
fibAction :: BotAction GlobalState
fibAction = BotAction
  { botActionName = "FibAction"
  , matchMsg = \_ msg -> "fib " `T.isPrefixOf` T.toLower msg
  , runAction = \event _ -> case event of
      MessageCreate msg -> unless (userIsBot (messageAuthor msg)) $ do
        let content = T.dropWhile (== ' ') $ T.drop 3 $ messageContent msg
        case readMaybe (T.unpack content) :: Maybe Integer of
          Nothing -> void $ sendMessageSafe "FibAction" (messageChannelId msg)
                        "Please provide a valid integer. Usage: fib 123"
          Just n -> do
            let result = fib n
            void $ sendMessageSafe "FibAction" (messageChannelId msg)
                   (T.pack $ "F(" ++ show n ++ ") = " ++ show result)
      _ -> return ()
  , actionMemBudget = Nothing
  }
