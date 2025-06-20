{-# LANGUAGE OverloadedStrings #-}
module Bot.RebaseAction (rebaseAction) where

import Discord
import Discord.Types
import Discord.Requests
import Control.Monad (void, unless)
import qualified Data.Text as T
import Bot.Types
import Data.List (elemIndex)

base64Digits :: String
base64Digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/"

digitToInt :: Char -> Maybe Int
digitToInt c = elemIndex c base64Digits

intToDigit :: Int -> Maybe Char
intToDigit n
  | n >= 0 && n < length base64Digits = Just (base64Digits !! n)
  | otherwise = Nothing

-- | Parses "rebase 1100 2 10" into (number, fromBase, toBase)
parseRebaseCommand :: T.Text -> Maybe (String, Int, Int)
parseRebaseCommand msg =
  case words (T.unpack msg) of
    ("rebase" : number : fromStr : toStr : _) ->
      case (reads fromStr, reads toStr) of
        ([(fromBase, "")], [(toBaseInner, "")]) -> Just (number, fromBase, toBaseInner)
        _ -> Nothing
    _ -> Nothing

-- | Converts a digit string from arbitrary base (including base 1) to Integer
parseDigits :: String -> Int -> Maybe Integer
parseDigits str 1
  | all (== '1') str = Just (toInteger (length str))
  | otherwise        = Nothing
parseDigits str base = go str 0
  where
    go [] acc = Just acc
    go (c:cs) acc = do
      val <- digitToInt c
      if val >= base then Nothing else go cs (acc * toInteger base + toInteger val)

-- | Converts an Integer to string in arbitrary base (including base 1)
toBase :: Integer -> Int -> Maybe String
toBase 0 _ = Just "0"
toBase n 1
  | n > 0     = Just (replicate (fromIntegral n) '1')
  | otherwise = Nothing
toBase n base = reverse <$> go n
  where
    go 0 = Just []
    go x = do
      d <- intToDigit (fromInteger (x `mod` toInteger base))
      rest <- go (x `div` toInteger base)
      return (d : rest)

-- | Complete base conversion
convertBase :: String -> Int -> Int -> Maybe String
convertBase digits fromB toB = do
  value <- parseDigits digits fromB
  toBase value toB

-- | Discord command
rebaseAction :: BotAction GlobalState
rebaseAction = BotAction
  { matchMsg = \_ msg -> "rebase" `T.isPrefixOf` T.toLower msg
  , runAction = \event _ -> case event of
      MessageCreate msg -> unless (userIsBot (messageAuthor msg)) $ do
        let input = messageContent msg
        case parseRebaseCommand input of
          Just (digits, fromB, toB)
            | validBase fromB && validBase toB ->
                case convertBase digits fromB toB of
                  Just result ->
                    void $ restCall $ CreateMessage (messageChannelId msg)
                      ("`" <> T.pack digits <> "` (base " <> tshow fromB <> ") = `" <> T.pack result <> "` (base " <> tshow toB <> ")")
                  Nothing ->
                    void $ restCall $ CreateMessage (messageChannelId msg) "Invalid digits for source base."
            | otherwise ->
                void $ restCall $ CreateMessage (messageChannelId msg) "Base must be between 1 and 64"
          Nothing ->
            void $ restCall $ CreateMessage (messageChannelId msg)
              "Usage: `rebase <number> <fromBase> <toBase>`"
      _ -> return ()
  }

validBase :: Int -> Bool
validBase b = b >= 1 && b <= 64

tshow :: Show a => a -> T.Text
tshow = T.pack . show
