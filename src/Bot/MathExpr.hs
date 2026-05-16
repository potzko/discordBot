{-|
Module      : Bot.MathExpr
Description : Implements a Discord bot command to evaluate basic arithmetic expressions
              including bitwise, modulo, and power operations.
              Example: "calc 1 + 2 * (3 + 4)" => 15
              Supports: + - * / % ** ^ | & ! !& !|
-}

{-# LANGUAGE OverloadedStrings #-}
module Bot.MathExpr (calcAction) where

import Discord.Types
import Control.Monad (void)
import qualified Data.Text as T
import Data.Char (isDigit, isSpace)
import Text.Read (readMaybe)
import Bot.Types
import Data.Bits ((.&.), (.|.), xor, complement)
import Bot.Util (sendMessageSafe)

calcAction :: BotAction GlobalState
calcAction = BotAction
  { botActionName = "MathExpr"
  , matchMsg = \_ txt -> 
      let cleaned = T.strip txt
          -- Only match if the entire message is a valid expression
          canParse = case evalExpr (T.unpack cleaned) of
                      Right _ -> True
                      Left _ -> False
          -- Check if it contains at least one operation (not just a number)
          hasOperation = any (`T.isInfixOf` cleaned) ["+", "-", "*", "/", "%", "**", "^", "|", "&", "!"]
      in canParse && hasOperation
  , runAction = \event _ -> case event of
      MessageCreate msg -> do
        let exprText = T.strip (messageContent msg)
        case evalExpr (T.unpack exprText) of
          Left _ -> return ()  -- Invalid expression, don't respond
          Right val -> 
            void $ sendMessageSafe "MathExpr" (messageChannelId msg) (T.pack ("Result: " ++ show val))
      _ -> return ()
  , actionMemBudget = Nothing
  }

evalExpr :: String -> Either String Integer
evalExpr s = case parseExpr (filter (not . isSpace) s) of
  Just (val, []) -> Right val
  _ -> Left "Invalid expression."

-- Grammar:
-- expr     ::= orExpr
-- orExpr   ::= xorExpr ( '|' xorExpr | '!|' xorExpr )*
-- xorExpr  ::= andExpr ( '^' andExpr )*
-- andExpr  ::= nandExpr ( '&' nandExpr )*
-- nandExpr ::= notExpr ( '!&' notExpr )*
-- notExpr  ::= '!' notExpr | addExpr
-- addExpr  ::= mulExpr ( ('+'|'-') mulExpr )*
-- mulExpr  ::= powExpr ( ('*'|'/'|'%') powExpr )*
-- powExpr  ::= atom ( '**' powExpr )*
-- atom     ::= number | '(' expr ')'

bitNot :: Integer -> Integer
bitNot x = complement x .&. 0xFFFFFFFF

parseExpr :: String -> Maybe (Integer, String)
parseExpr = parseOr

parseOr :: String -> Maybe (Integer, String)
parseOr = chainl1 parseXor orOp
  where orOp ('|':rest) = Just ((.|.), rest)
        orOp ('!':'|':rest) = Just ((\a b -> bitNot (a .|. b)), rest)
        orOp _ = Nothing

parseXor :: String -> Maybe (Integer, String)
parseXor = chainl1 parseAnd xorOp
  where xorOp ('^':rest) = Just (xor, rest)
        xorOp _ = Nothing

parseAnd :: String -> Maybe (Integer, String)
parseAnd = chainl1 parseNand andOp
  where andOp ('&':rest) = Just ((.&.), rest)
        andOp _ = Nothing

parseNand :: String -> Maybe (Integer, String)
parseNand = chainl1 parseNot nandOp
  where nandOp ('!':'&':rest) = Just ((\a b -> bitNot (a .&. b)), rest)
        nandOp _ = Nothing

parseNot :: [Char] -> Maybe (Integer, String)
parseNot ('!':rest) = do
  (v, rest') <- parseNot rest
  return (bitNot v, rest')
parseNot s = parseAdd s

parseAdd :: String -> Maybe (Integer, String)
parseAdd = chainl1 parseMul addOp
  where addOp ('+':rest) = Just ((+), rest)
        addOp ('-':rest) = Just ((-), rest)
        addOp _ = Nothing

parseMul :: String -> Maybe (Integer, String)
parseMul = chainl1 parsePow mulOp
  where mulOp ('*':'*': _rest) = Nothing  -- exclude power here
        mulOp ('*':rest) = Just ((*), rest)
        mulOp ('/':rest) = Just (div, rest)
        mulOp ('%':rest) = Just (mod, rest)
        mulOp _ = Nothing

parsePow :: String -> Maybe (Integer, String)
parsePow = chainr1 parseAtom powOp
  where powOp ('*':'*':rest) = Just ((^), rest)
        powOp _ = Nothing

parseAtom :: String -> Maybe (Integer, String)
parseAtom ('-':rest) = do
  (v, rest') <- parseAtom rest
  return (-v, rest')

parseAtom ('(':rest) = do
  (e, rest') <- parseExpr rest
  case rest' of
    (')':rest'') -> Just (e, rest'')
    _ -> Nothing

parseAtom s =
  let (numStr, rest) = span isDigit s
  in case readMaybe numStr of
    Just n  -> Just (n, rest)
    Nothing -> Nothing

-- Helper: chained left-associative parser
chainl1 :: (String -> Maybe (Integer, String)) -> (String -> Maybe (Integer -> Integer -> Integer, String)) -> String -> Maybe (Integer, String)
chainl1 p op s = do
  (x, rest) <- p s
  chain x rest
  where
    chain acc rest = case op rest of
      Just (f, rest') -> do
        (y, rest'') <- p rest'
        chain (f acc y) rest''
      Nothing -> Just (acc, rest)

-- Helper: chained right-associative parser
chainr1 :: (String -> Maybe (Integer, String)) -> (String -> Maybe (Integer -> Integer -> Integer, String)) -> String -> Maybe (Integer, String)
chainr1 p op s = do
  (x, rest) <- p s
  case op rest of
    Just (f, rest') -> do
      (y, rest'') <- chainr1 p op rest'
      return (f x y, rest'')
    Nothing -> Just (x, rest)
