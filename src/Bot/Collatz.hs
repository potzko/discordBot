{-|
Module      : Bot.Collatz
Description : Implements the "collatz N" command — shows the 3n+1 sequence and stopping time.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Bot.Collatz (collatzAction) where

import Discord.Types
import Control.Monad (void, unless)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Bot.Types
import Bot.Util (sendMessageSafe)

-- | Collatz sequence from n down to 1 (n must be >= 1).
collatzSeq :: Integer -> [Integer]
collatzSeq n
  | n <= 1    = [n]
  | even n    = n : collatzSeq (n `div` 2)
  | otherwise = n : collatzSeq (3 * n + 1)

-- | Discord command handler for "collatz N".
collatzAction :: BotAction GlobalState
collatzAction = BotAction
  { botActionName = "Collatz"
  , matchMsg = \_ msg -> "collatz " `T.isPrefixOf` T.toLower msg
  , runAction = \event _ -> case event of
      MessageCreate msg -> unless (userIsBot (messageAuthor msg)) $ do
        let content = T.strip $ T.dropWhile (/= ' ') (messageContent msg)
        case readMaybe (T.unpack content) :: Maybe Integer of
          Nothing ->
            void $ sendMessageSafe "Collatz" (messageChannelId msg)
              "Please provide a positive integer. Usage: collatz 27"
          Just n
            | n < 1 ->
                void $ sendMessageSafe "Collatz" (messageChannelId msg)
                  "Please provide a positive integer."
            | otherwise -> do
                let seqList = collatzSeq n
                    stop   = length seqList - 1
                    peakV  = maximum seqList
                void $ sendMessageSafe "Collatz" (messageChannelId msg)
                       (formatOutput n stop peakV seqList)
      _ -> return ()
  , actionMemBudget = Nothing
  }

-- | Format the Collatz output, truncating long sequences for Discord's 2000 char cap.
formatOutput :: Integer -> Int -> Integer -> [Integer] -> T.Text
formatOutput n stop peakV seqList =
  let header = T.pack $ "Collatz(" ++ show n
                     ++ "): stopping time = " ++ show stop
                     ++ ", peak = " ++ show peakV
      body
        | length seqList <= 60 = renderArrow seqList
        | otherwise =
            renderArrow (take 25 seqList)
              <> " -> ... -> "
              <> renderArrow (drop (length seqList - 25) seqList)
  in header <> "\n" <> body

renderArrow :: [Integer] -> T.Text
renderArrow = T.intercalate " -> " . map (T.pack . show)
