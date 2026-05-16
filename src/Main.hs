{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import Discord
import Discord.Types
import Discord.Requests (ChannelRequest(CreateMessage))
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import qualified Control.Exception as E
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Int (Int64)
import System.Environment (getEnv)
import System.Timeout (timeout)
import Configuration.Dotenv (loadFile, defaultConfig)
import GHC.Conc (setAllocationCounter, enableAllocationLimit, disableAllocationLimit)

import Bot.Types (GlobalState(..), BotAction(..))
import Bot.PingPong (pingAction, pongAction)
import Bot.Dice (diceAction)
import Bot.MathExpr (calcAction)
import Bot.FactorialDetector (factorialAction)
import Bot.PolinomeAction (polinoeAction)
import Bot.FibAction (fibAction)
import Bot.RebaseAction (rebaseAction)
import Bot.HelpAction (helpAction, mathHelpAction)
import Bot.Collatz (collatzAction)
import Bot.Choose (chooseAction, coinflipAction)
import Bot.Factorize (factorizeAction)
import Bot.PalindromeDetector (palindromeAction)
import Bot.NumberProperties (numberPropertyAction)

-- | Per-action wall-clock cap (5 seconds).
actionTimeoutMicros :: Int
actionTimeoutMicros = 5 * 1000000

-- | Default per-action allocation budget (100 MB). Individual actions can
--   override via `actionMemBudget` on their `BotAction` (e.g. Factorize asks
--   for 500 MB).
defaultMemBudget :: Int64
defaultMemBudget = 100 * 1024 * 1024

data ActionOutcome = Ok | TimedOut | OutOfMemory

-- | Handles incoming Discord events; runs the first matching action under
--   both a wall-clock and an allocation cap.
handleEvent :: [BotAction GlobalState] -> GlobalState -> Event -> DiscordHandler ()
handleEvent actions state ev@(MessageCreate msg) = do
  let msgText = messageContent msg
      chan = messageChannelId msg
  unless (userIsBot (messageAuthor msg)) $
    handleActions actions msgText chan
  where
    handleActions [] _ _ = return ()
    handleActions (b@BotAction{..}:rest) msgText chan =
      if matchMsg state msgText
        then runWithBounds b chan (runAction ev state)
        else handleActions rest msgText chan
handleEvent _ _ _ = return ()

-- | Run a handler under both a wall-clock timeout and a per-thread allocation
--   limit. The allocation budget comes from the action's own `actionMemBudget`
--   (falling back to the global default).
runWithBounds :: BotAction GlobalState -> ChannelId -> DiscordHandler () -> DiscordHandler ()
runWithBounds botAction chan handler = do
  env <- ask
  outcome <- liftIO $ do
    let budget = maybe defaultMemBudget id (actionMemBudget botAction)
    setAllocationCounter budget
    enableAllocationLimit
    result <- E.try (timeout actionTimeoutMicros (runReaderT handler env))
                `E.finally` disableAllocationLimit
    return $ case result of
      Right (Just _) -> Ok
      Right Nothing  -> TimedOut
      Left (_ :: E.AllocationLimitExceeded) -> OutOfMemory
  case outcome of
    Ok          -> return ()
    TimedOut    -> void $ restCall $ CreateMessage chan "That took too long."
    OutOfMemory -> void $ restCall $ CreateMessage chan "That used too much memory."

-- | Entry point of the Discord bot
main :: IO ()
main = do
  loadFile defaultConfig
  token <- getEnv "DISCORD_TOKEN"
  let globalState = GlobalState
      actions =
        [ mathHelpAction      -- before helpAction so "help -- math" wins
        , helpAction
        -- explicit commands
        , pingAction
        , pongAction
        , diceAction
        , calcAction
        , polinoeAction
        , fibAction
        , rebaseAction
        , collatzAction
        , factorizeAction
        , chooseAction
        , coinflipAction
        -- text-based detectors
        , factorialAction
        , palindromeAction
        -- number-based detectors
        , numberPropertyAction
        ]
  err <- runDiscord $ def
    { discordToken = T.pack token
    , discordOnStart = liftIO $ putStrLn "Bot started"
    , discordOnEvent = handleEvent actions globalState
    }
  TIO.putStrLn err
