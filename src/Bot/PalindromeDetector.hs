{-|
Module      : Bot.PalindromeDetector
Description : Detects when an entire message (after normalization) is a palindrome.
              Runs in O(N) — just normalize, reverse, compare.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Bot.PalindromeDetector (palindromeAction) where

import Discord.Types
import Control.Monad (void, unless, when)
import qualified Data.Text as T
import Data.Char (isAlphaNum, toLower)
import Bot.Types
import Bot.Util (sendMessageSafe)

-- | Strip non-alphanumeric characters and lowercase. So "A man, a plan, a canal: Panama"
--   becomes "amanaplanacanalpanama".
normalize :: T.Text -> T.Text
normalize = T.map toLower . T.filter isAlphaNum

-- | Minimum normalized length to consider a palindrome interesting.
--   Below this, accidental matches dominate ("aa", "aba", etc.).
minLen :: Int
minLen = 5

isPalindrome :: T.Text -> Bool
isPalindrome t = t == T.reverse t

-- | Reject single-char repeats like "aaaaa" — true palindromes but boring.
hasVariety :: T.Text -> Bool
hasVariety t = case T.uncons t of
  Nothing      -> False
  Just (c, cs) -> T.any (/= c) cs

palindromeAction :: BotAction GlobalState
palindromeAction = BotAction
  { botActionName = "PalindromeDetector"
  , matchMsg = \_ txt ->
      let n = normalize txt
      in T.length n >= minLen && hasVariety n && isPalindrome n
  , runAction = \event _ -> case event of
      MessageCreate msg -> unless (userIsBot (messageAuthor msg)) do
        let n = normalize (messageContent msg)
        when (T.length n >= minLen && hasVariety n && isPalindrome n) $
          void $ sendMessageSafe "PalindromeDetector" (messageChannelId msg)
                   (T.pack ("That's a palindrome (" ++ show (T.length n) ++ " chars)!"))
      _ -> return ()
  , actionMemBudget = Nothing
  }
