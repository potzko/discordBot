{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import Discord
import Discord.Types
import Discord.Requests (ChannelRequest(CreateMessage))
import Control.Monad (when, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Environment (getEnv)
import System.Timeout (timeout)
import Configuration.Dotenv (loadFile, defaultConfig)
import Data.Maybe (isNothing)

import Bot.Types (GlobalState(..), BotAction(..))
import Bot.PingPong (pingAction, pongAction)
import Bot.Dice (diceAction)
import Bot.MathExpr (calcAction)
import Bot.FactorialDetector (factorialAction)
import Bot.PolinomeAction (polinoeAction)
import Bot.FibAction (fibAction)
import Bot.RebaseAction (rebaseAction)
import Bot.PrimeDetector (primeAction)

-- | Handles incoming Discord events and applies a timeout per action
handleEvent :: [BotAction GlobalState] -> GlobalState -> Event -> DiscordHandler ()
handleEvent actions state ev@(MessageCreate msg) = do
  let msgText = messageContent msg
      chan = messageChannelId msg
  unless (userIsBot (messageAuthor msg)) $
    handleActions actions msgText chan
  where
    handleActions [] _ _ = return ()  -- No more actions to try
    handleActions (BotAction{..}:rest) msgText chan = do
      if matchMsg state msgText
        then runWithTimeoutWithLimit botActionName chan (5 * 10^(6 :: Int)) (runAction ev state)  -- Execute this action and stop
        else handleActions rest msgText chan  -- Try next action
handleEvent _ _ _ = return ()

-- | Like runWithTimeout, but intercepts outgoing messages for length
runWithTimeoutWithLimit :: String -> ChannelId -> Int -> DiscordHandler () -> DiscordHandler ()
runWithTimeoutWithLimit _actionName chan micros handler = do
  env <- ask
  result <- liftIO $ timeout micros (runReaderT handler env)
  when (isNothing result) $
    void $ restCall $ CreateMessage chan "That took too long."

-- | Entry point of the Discord bot
main :: IO ()
main = do
  loadFile defaultConfig
  token <- getEnv "DISCORD_TOKEN"
  let globalState = GlobalState
      actions =
        [ pingAction
        , pongAction
        , diceAction
        , calcAction
        , factorialAction
        , polinoeAction
        , fibAction
        , rebaseAction
        , primeAction
        ]
  err <- runDiscord $ def
    { discordToken = T.pack token
    , discordOnStart = liftIO $ putStrLn "Bot started"
    , discordOnEvent = handleEvent actions globalState
    }
  TIO.putStrLn err
