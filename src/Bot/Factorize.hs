{-|
Module      : Bot.Factorize
Description : Discord command "factorize N" / "factorise N". Pure factorization
              lives in Bot.Factorization; this module only handles the Discord side
              and enforces an internal 2-second timeout (the global 5s timeout in
              Main is a safety net behind it).
-}

{-# LANGUAGE OverloadedStrings #-}
module Bot.Factorize (factorizeAction) where

import Discord.Types
import Control.Monad (void, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (evaluate)
import qualified Data.Text as T
import Text.Read (readMaybe)
import System.Timeout (timeout)

import Bot.Types
import Bot.Util (sendMessageSafe)
import Bot.Factorization (factorize)

-- | 2-second internal timeout, in microseconds.
factorizeTimeoutMicros :: Int
factorizeTimeoutMicros = 2000000

factorizeAction :: BotAction GlobalState
factorizeAction = BotAction
  { botActionName = "Factorize"
  , matchMsg = \_ msg ->
      let l = T.toLower msg
      in "factorize " `T.isPrefixOf` l || "factorise " `T.isPrefixOf` l
  , runAction = \event _ -> case event of
      MessageCreate msg -> unless (userIsBot (messageAuthor msg)) $ do
        let content = T.strip $ T.dropWhile (/= ' ') (messageContent msg)
        case readMaybe (T.unpack content) :: Maybe Integer of
          Nothing ->
            void $ sendMessageSafe "Factorize" (messageChannelId msg)
              "Usage: factorize N (where N >= 2)"
          Just n
            | n < 2 ->
                void $ sendMessageSafe "Factorize" (messageChannelId msg)
                  "Please provide an integer >= 2."
            | otherwise -> do
                result <- liftIO $ timeout factorizeTimeoutMicros $ do
                  let formatted = formatFactorization n (factorize n)
                  -- T.length on a strict Text fully forces it, which forces the
                  -- factorize computation chained inside the formatted string.
                  _ <- evaluate (T.length formatted)
                  return formatted
                case result of
                  Nothing ->
                    void $ sendMessageSafe "Factorize" (messageChannelId msg)
                      ("Factorization of " <> T.pack (show n) <> " timed out (2s).")
                  Just str ->
                    void $ sendMessageSafe "Factorize" (messageChannelId msg) str
      _ -> return ()
  , actionMemBudget = Just (500 * 1024 * 1024)
  }

formatFactorization :: Integer -> [(Integer, Int)] -> T.Text
formatFactorization n [(p, 1)] | p == n =
  T.pack (show n) <> " is prime."
formatFactorization n factors =
  T.pack (show n) <> " = " <> T.intercalate " * " (map showFactor factors)
  where
    showFactor (p, 1) = T.pack (show p)
    showFactor (p, k) = T.pack (show p ++ "^" ++ show k)
