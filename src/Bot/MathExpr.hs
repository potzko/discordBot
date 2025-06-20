{-|
Module      : Bot.MathExpr
Description : Implements a Discord bot command to evaluate basic arithmetic expressions with parentheses.
              Example: "calc 1 + 2 * (3 + 4)" => 15
|-}

{-# LANGUAGE OverloadedStrings #-}
module Bot.MathExpr (calcAction) where

import Discord
import Discord.Types
import Discord.Requests
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Char (isDigit, isSpace)
import Text.Read (readMaybe)

import Bot.Types

-- | Top-level Discord command dispatcher for "calc <expr>"
calcAction :: BotAction GlobalState
calcAction = BotAction
  { matchMsg = \_ txt -> "calc" `T.isPrefixOf` T.toLower txt
  , runAction = \event _ -> case event of
      MessageCreate msg -> do
        let exprText = T.strip $ T.dropWhile (/= ' ') (messageContent msg)
        case evalExpr (T.unpack exprText) of
          Left err -> void $ restCall (CreateMessage (messageChannelId msg) (T.pack err))
          Right val -> void $ restCall (CreateMessage (messageChannelId msg) (T.pack ("Result: " ++ show val)))
      _ -> return ()
  }

-- | Evaluate a mathematical expression as a string, returning either an error or result
--   This uses a simple recursive descent parser
evalExpr :: String -> Either String Int
evalExpr s = case parseExpr (filter (not . isSpace) s) of
  Just (val, []) -> Right val
  _ -> Left "Invalid expression."

-- Grammar:
-- expr   ::= term (('+'|'-') term)*
-- term   ::= factor (('*'|'/') factor)*
-- factor ::= number | '(' expr ')'

parseExpr :: String -> Maybe (Int, String)
parseExpr s = do
  (t, rest) <- parseTerm s
  parseExpr' t rest
  where
    parseExpr' acc ('+':rest) = do
      (t, rest') <- parseTerm rest
      parseExpr' (acc + t) rest'
    parseExpr' acc ('-':rest) = do
      (t, rest') <- parseTerm rest
      parseExpr' (acc - t) rest'
    parseExpr' acc rest = Just (acc, rest)

parseTerm :: String -> Maybe (Int, String)
parseTerm s = do
  (f, rest) <- parseFactor s
  parseTerm' f rest
  where
    parseTerm' acc ('*':rest) = do
      (f, rest') <- parseFactor rest
      parseTerm' (acc * f) rest'
    parseTerm' acc ('/':rest) = do
      (f, rest') <- parseFactor rest
      if f == 0 then Nothing else parseTerm' (acc `div` f) rest'
    parseTerm' acc rest = Just (acc, rest)

parseFactor :: String -> Maybe (Int, String)
parseFactor ('(':rest) = do
  (e, rest') <- parseExpr rest
  case rest' of
    (')':rest'') -> Just (e, rest'')
    _ -> Nothing
parseFactor s =
  let (numStr, rest) = span isDigit s
  in case readMaybe numStr of
    Just n  -> Just (n, rest)
    Nothing -> Nothing
