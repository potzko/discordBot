{-|
Module      : Bot.Dice
Description : Implements a Discord bot command to evaluate and roll named and nested dice expressions.
              Allows grouping, naming, and computing totals for individual sets of dice.
|-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
module Bot.Dice (diceAction) where

import Discord
import Discord.Types
import Discord.Requests
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace)
import System.Random (randomRIO)
import Text.Regex.TDFA ((=~))

import Bot.Types

-- | AST for parsed dice expressions
--   A roll expression is either a group with an optional name,
--   or a simple dice roll of the form NdM (e.g., 2d6).
data RollExpr
  = RollGroup (Maybe String) [RollExpr]  -- ^ Named group: name(...), or unnamed: (...)
  | DiceRoll Int Int                     -- ^ Roll of N dice with M sides (NdM)
  deriving (Show)

-- | Result of evaluating a dice expression
--   Contains either a named group result or individual dice roll results.
data RollResult
  = ResultGroup { name :: Maybe String, total :: Int, children :: [RollResult] }  -- ^ Group with subresults and total
  | ResultDice  { label :: String, rolls :: [Int] }                                -- ^ Simple dice roll and outcomes
  deriving (Show)

-- | Discord action for "roll <expression>"
--   Parses the command, evaluates the dice rolls, and sends a formatted message back.
diceAction :: BotAction GlobalState
diceAction = BotAction
  { matchMsg = \_ txt -> "roll" `T.isPrefixOf` T.toLower txt
  , runAction = \event _ -> case event of
      MessageCreate msg -> do
        let content = T.strip $ T.dropWhile (/= ' ') (messageContent msg)
        case parseExprs (T.unpack content) of
          Nothing -> void $ restCall (CreateMessage (messageChannelId msg) "Invalid roll syntax.")
          Just exprs -> do
            results <- liftIO $ mapM evalExpr exprs
            let innerTotal = sum (map getTotal results)
            let output = T.unlines (formatResult <$> results)
            void $ restCall (CreateMessage (messageChannelId msg)
              (T.concat ["Roll: (total = ", T.pack (show innerTotal), ")\n", output]))
      _ -> return ()
  }

-- | Extract total value from a RollResult node
getTotal :: RollResult -> Int
getTotal (ResultGroup _ t _) = t
getTotal (ResultDice _ rs) = sum rs

-- | Format a RollResult into a structured, indented Text block
formatResult :: RollResult -> T.Text
formatResult (ResultDice lbl rs) = T.concat [T.pack lbl, ": ", T.pack (show rs)]
formatResult (ResultGroup Nothing t innerChildren) =
  T.concat ["(total = ", T.pack (show t), ")\n"] <> T.unlines (map indentBlock innerChildren)
formatResult (ResultGroup (Just n) t innerChildren) =
  T.concat [T.pack n, ": (total = ", T.pack (show t), ")\n"] <> T.unlines (map indentBlock innerChildren)

-- | Helper to indent all lines of a result block
indentBlock :: RollResult -> T.Text
indentBlock = indent . formatResult

-- | Add tab-indentation to each line of a Text block
indent :: T.Text -> T.Text
indent = T.unlines . map ("\t" <>) . T.lines

-- | Evaluate a RollExpr tree into actual dice values
--   Aggregates and structures results recursively.
evalExpr :: RollExpr -> IO RollResult
evalExpr (DiceRoll count sides) = do
  rs <- mapM (const $ randomRIO (1, sides)) [1..count]
  return $ ResultDice (show count ++ "d" ++ show sides) rs
evalExpr (RollGroup innerName xs) = do
  innerChildren <- mapM evalExpr xs
  return $ ResultGroup innerName (sum $ map getTotal innerChildren) innerChildren

-- | Parse the top-level list of expressions from input tokens
--   Ensures the full input is consumed
parseExprs :: String -> Maybe [RollExpr]
parseExprs s = case parseMany (filter (not . null) $ wordsLike s) of
  (Just xs, []) -> Just xs
  _ -> Nothing

-- | Tokenizer for dice syntax: splits strings into symbols and words
wordsLike :: String -> [String]
wordsLike [] = []
wordsLike (c:cs)
  | isSpace c = wordsLike cs
  | c `elem` ("()" :: String) = [c] : wordsLike cs
  | otherwise = let (tok, rest) = span isPart (c:cs)
                in tok : wordsLike rest
  where isPart x = isAlphaNum x || x == '_' || x == 'd'

-- | Recursive parser for zero or more expressions, stopping on ')'
parseMany :: [String] -> (Maybe [RollExpr], [String])
parseMany [] = (Just [], [])
parseMany (")":xs) = (Just [], ")":xs)
parseMany toks =
  let (m1, rest1) = parseOne toks
  in case m1 of
       Nothing -> (Nothing, rest1)
       Just r1 -> let (mrest, rest2) = parseMany rest1
                   in case mrest of
                        Nothing -> (Nothing, rest2)
                        Just rs -> (Just (r1:rs), rest2)

-- | Parser for a single RollExpr token or group
parseOne :: [String] -> (Maybe RollExpr, [String])
parseOne [] = (Nothing, [])
parseOne (tok:"(":rest) =
  let (minside, rest') = parseMany rest
  in case minside of
       Nothing -> (Nothing, rest')
       Just inside -> case rest' of
         ")":more -> (Just (RollGroup (Just tok) inside), more)
         _ -> (Nothing, rest')
parseOne ("(":rest) =
  let (minside, rest') = parseMany rest
  in case minside of
       Just inside -> case rest' of
         ")":more -> (Just (RollGroup Nothing inside), more)
         _ -> (Nothing, rest')
       Nothing -> (Nothing, rest')
parseOne (tok:rest)
  | Just (n, s) <- parseDie tok = (Just (DiceRoll n s), rest)
  | otherwise = (Nothing, rest)

-- | Recognize dice notation like "2d6", "d20", etc.
--   The first group is optional (defaulting to 1).
parseDie :: String -> Maybe (Int, Int)
parseDie s = case (s =~ ("^([0-9]*)d([0-9]+)$" :: String)) :: (String, String, String, [String]) of
  (_, _, _, [a, b]) -> Just (read (if null a then "1" else a), read b)
  _ -> Nothing
