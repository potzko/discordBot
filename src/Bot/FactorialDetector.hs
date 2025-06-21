{-|
Module      : Bot.Factorial
Description : Detects naturally occurring factorials like "12!" in messages and replies with the result.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Bot.FactorialDetector (factorialAction) where

import Discord
import Discord.Types
import Discord.Requests
import Control.Monad (void, unless)
import qualified Data.Text as T
import Text.Regex.TDFA ((=~))
import Bot.Types

-- | The passive factorial detection action.
--   When a message contains a number followed by '!', the bot replies with the factorial.
--   For large values, it estimates the number of digits.
factorialAction :: BotAction GlobalState
factorialAction = BotAction
  { matchMsg = \_ msg -> any isFactorial (T.words msg)  -- Only run if message contains factorial-looking tokens
  , runAction = \event _ -> case event of
      MessageCreate msg -> unless (userIsBot (messageAuthor msg)) do
        let txt = messageContent msg
        let facts = filter isFactorial (T.words txt)
        case facts of
          [] -> return ()
          _ -> do
            let numbers = map (read . T.unpack . T.init) facts
            let results = [(n, decideFactorial n) | n <- numbers]
            let formatted = T.unlines [formatFact n res | (n, res) <- results]
            unless (T.null formatted) do
              void $ restCall (CreateMessage (messageChannelId msg) formatted)
      _ -> return ()
  }

-- | Checks if a word looks like a factorial (e.g., "5!")
isFactorial :: T.Text -> Bool
isFactorial word = word =~ ("^[0-9]+!$" :: String)

-- | Max safe value before even estimation becomes unreliable
maxSafeEstimate :: Integer
maxSafeEstimate = 10 ^ (18 :: Integer)

-- | First estimates digit count; computes exact value only if it is small.
--   Returns Left value if computed, Right digit count otherwise.
decideFactorial :: Integer -> Either Integer Int
decideFactorial n
  | n > maxSafeEstimate = Right (-1)  -- sentinel for absurdly large
  | digitEst <= 30 = Left (product [1..n])
  | otherwise = Right digitEst
  where digitEst = digitsKamenetsky n

-- | Uses Kamenetsky’s formula to estimate the number of digits in n!
digitsKamenetsky :: Integer -> Int
digitsKamenetsky n
  | n == 0 || n == 1 = 1
  | otherwise =
      floor (n' * logBase 10 (n' / exp 1) + logBase 10 (2 * pi * n') / 2) + 1
  where n' = fromIntegral n :: Double

-- | Formats the factorial result based on size
formatFact :: Integer -> Either Integer Int -> T.Text
formatFact n (Left val) = T.pack (show n ++ "! = " ++ show val)
formatFact n (Right (-1)) = T.pack (show n ++ "! is too large to estimate.")
formatFact n (Right digits) = T.pack (show n ++ "! has " ++ show digits ++ " digits")
