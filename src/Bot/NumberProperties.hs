{-|
Module      : Bot.NumberProperties
Description : Detector that scans messages for numbers and reports each number's properties.
              For each detected integer, every registered property runs and the holding
              labels are appended. Add a new property by dropping a file in
              Bot/NumberProperties/ and registering it in `allProperties`.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Bot.NumberProperties (numberPropertyAction) where

import Discord.Types
import Control.Monad (void, unless)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Maybe (mapMaybe)
import Data.Char (isDigit)

import Bot.Types
import Bot.Util (sendMessageSafe)

import Bot.NumberProperties.Types (Property(..))
import qualified Bot.NumberProperties.Prime        as Prime
import qualified Bot.NumberProperties.Happy        as Happy
import qualified Bot.NumberProperties.Perfect      as Perfect
import qualified Bot.NumberProperties.Abundant     as Abundant
import qualified Bot.NumberProperties.Narcissistic as Narcissistic
import qualified Bot.NumberProperties.Fibonacci    as Fibonacci
import qualified Bot.NumberProperties.Triangular   as Triangular

-- | All registered properties, in the order they appear in replies.
allProperties :: [Property]
allProperties =
  [ Prime.property
  , Happy.property
  , Perfect.property
  , Abundant.property
  , Narcissistic.property
  , Fibonacci.property
  , Triangular.property
  ]

-- | Labels of every property that holds for n. Properties whose
--   `propertyApplies` returns False on n are silently skipped — that's how
--   we keep slow checks (Perfect/Abundant for huge n) from starving the
--   fast ones (Prime/Happy). Properties may return their own custom label
--   (e.g. Fibonacci returns `fibonacci(123)` instead of `fibonacci`).
labelsFor :: Integer -> [T.Text]
labelsFor n =
  [ label
  | p <- allProperties
  , propertyApplies p n
  , Just label <- [propertyLabel p n]
  ]

-- | Extract bare integers (>= 2) from a message.
extractNumbers :: T.Text -> [Integer]
extractNumbers =
  filter (>= 2)
    . mapMaybe (either (const Nothing) (Just . fst) . TR.decimal)
    . T.words

hasDigit :: T.Text -> Bool
hasDigit = T.any isDigit

numberPropertyAction :: BotAction GlobalState
numberPropertyAction = BotAction
  { botActionName = "NumberProperties"
  , matchMsg = \_ msg ->
      hasDigit msg && any (not . null . labelsFor) (extractNumbers msg)
  , runAction = \event _ -> case event of
      MessageCreate msg -> unless (userIsBot (messageAuthor msg)) do
        let nums = extractNumbers (messageContent msg)
            hits = [(n, ls) | n <- nums, let ls = labelsFor n, not (null ls)]
        case hits of
          [] -> return ()
          _  -> void $ sendMessageSafe "NumberProperties" (messageChannelId msg)
                         (T.unlines (map formatHit hits))
      _ -> return ()
  , actionMemBudget = Nothing
  }

formatHit :: (Integer, [T.Text]) -> T.Text
formatHit (n, props) = T.pack (show n) <> ": " <> T.intercalate ", " props
